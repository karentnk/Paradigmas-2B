(ns atividade.integrador
  (:gen-class))

(def nota-minima 7.0)

(defn- status-aluno [aluno]
  (if (>= (:nota aluno) nota-minima)
    "Aprovado"
    "Reprovado"))

(defn- com-status [aluno]
  (assoc aluno :status (status-aluno aluno)))

(defn- media-turma [alunos]
  (if (empty? alunos)
    0.0
    (let [notas (map :nota alunos)]
      (/ (reduce + notas) (count notas)))))

(defn- ler-nota [prompt]
  (println prompt)
  (try
    (Double/parseDouble (read-line))
    (catch Exception _
      (println "Entrada inválida. Por favor, digite um número (ex: 8.5).")
      nil)))

(defn- cadastrar [alunos]
  (println "\n--- 1. Cadastrar Alunos ---")
  (println "(Deixe o nome em branco para voltar ao menu)")
  (loop [acc alunos]
    (println "Digite o nome do aluno:")
    (let [nome (read-line)]
      (if (empty? nome)
        acc
        (if-let [nota (ler-nota (str "Digite a nota de " nome ":"))]
          (let [aluno {:nome nome :nota nota}]
            (recur (conj acc aluno)))
          (recur acc))))))

(defn- relatorio [alunos]
  (println "\n--- 2. Relatório de Notas ---")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-status (map com-status alunos)
          aprovados (filter #(= (:status %) "Aprovado") alunos-status)]

      (println "Alunos Aprovados:")
      (if (empty? aprovados)
        (println "Nenhum aluno aprovado.")
        (doseq [aluno aprovados]
          (println (str "\tNome: " (:nome aluno) ", Nota: " (:nota aluno)))))

      (println "---")
      (println (format "Média Geral da Turma: %.2f" (media-turma alunos)))))
  alunos)

(defn- estatisticas [alunos]
  (println "\n--- 3. Estatísticas Gerais ---")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [total (count alunos)
          alunos-status (map com-status alunos)
          num-aprov (count (filter #(= (:status %) "Aprovado") alunos-status))
          num-reprov (- total num-aprov)
          notas (map :nota alunos)
          nota-max (if (empty? notas) 0 (apply max notas))
          nota-min (if (empty? notas) 0 (apply min notas))
          media (media-turma alunos)]

      (println (str "Total de alunos cadastrados: " total))
      (println (str "Número de aprovados: " num-aprov))
      (println (str "Número de reprovados: " num-reprov))
      (println (str "Maior nota: " nota-max))
      (println (str "Menor nota: " nota-min))
      (println (format "Média geral da turma: %.2f" media))))
  alunos)

(defn- buscar [alunos]
  (println "\n--- 4. Buscar Aluno (Extra) ---")
  (println "Digite o nome (ou parte do nome) do aluno:")
  (let [termo (clojure.string/lower-case (read-line))
        resultados (filter #(clojure.string/includes?
                             (clojure.string/lower-case (:nome %))
                             termo)
                           alunos)]
    (if (empty? resultados)
      (println "Nenhum aluno encontrado com esse nome.")
      (doseq [aluno resultados]
        (let [aluno-status (com-status aluno)]
          (println (str "\tNome: " (:nome aluno-status)
                        ", Nota: " (:nota aluno-status)
                        ", Status: " (:status aluno-status)))))))
  alunos)

(defn- menu []
  (println "\n=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatório de Notas")
  (println "3 - Estatísticas Gerais")
  (println "4 - Buscar Aluno (Extra)")
  (println "0 - Sair")
  (println "Escolha uma opção:"))

(defn -main [& args]
  (loop [alunos []]
    (menu)
    (let [op (read-line)]
      (cond
        (= op "1") (recur (cadastrar alunos))
        (= op "2") (recur (relatorio alunos))
        (= op "3") (recur (estatisticas alunos))
        (= op "4") (recur (buscar alunos))
        (= op "0") (println "Saindo do programa...")
        :else (do
                (println "Opção inválida. Tente novamente.")
                (recur alunos))))))
