(ns atividade.integrador
  (:gen-class))

(def NOTA-MINIMA-APROVACO 7.0)

(defn- get-status [aluno]
  (if (>= (:nota aluno) NOTA-MINIMA-APROVACO)
    "Aprovado"
    "Reprovado"))

(defn- adicionar-status [aluno]
  (assoc aluno :status (get-status aluno)))

(defn- calcular-media [alunos]
  (if (empty? alunos)
    0.0
    (let [notas (map :nota alunos)]
      (/ (reduce + notas) (count notas)))))

(defn- read-double [prompt]
  (println prompt)
  (try
    (Double/parseDouble (read-line))
    (catch Exception _
      (println "Entrada inválida. Por favor, digite um número (ex: 8.5).")
      nil)))

(defn- cadastrar-alunos [alunos]
  (println "\n--- 1. Cadastrar Alunos ---")
  (println "(Deixe o nome em branco para voltar ao menu)")
  (loop [alunos-atuais alunos]
    (println "Digite o nome do aluno:")
    (let [nome (read-line)]
      (if (empty? nome)
        alunos-atuais
        (if-let [nota (read-double (str "Digite a nota de " nome ":"))]
          (let [novo-aluno {:nome nome :nota nota}]
            (recur (conj alunos-atuais novo-aluno)))
          (recur alunos-atuais))))))

(defn- relatorio-notas [alunos]
  (println "\n--- 2. Relatório de Notas ---")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-com-status (map adicionar-status alunos)
          aprovados (filter #(= (:status %) "Aprovado") alunos-com-status)]

      (println "Alunos Aprovados:")
      (if (empty? aprovados)
        (println "Nenhum aluno aprovado.")
        (doseq [aluno aprovados]
          (println (str "\tNome: " (:nome aluno) ", Nota: " (:nota aluno)))))

      (println "---")
      (println (format "Média Geral da Turma: %.2f" (calcular-media alunos)))))
  alunos)

(defn- estatisticas-gerais [alunos]
  (println "\n--- 3. Estatísticas Gerais ---")
  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [total-alunos (count alunos)
          alunos-com-status (map adicionar-status alunos)
          aprovados (count (filter #(= (:status %) "Aprovado") alunos-com-status))
          reprovados (- total-alunos aprovados)
          notas (map :nota alunos)
          maior-nota (if (empty? notas) 0 (apply max notas))
          menor-nota (if (empty? notas) 0 (apply min notas))
          media (calcular-media alunos)]

      (println (str "Total de alunos cadastrados: " total-alunos))
      (println (str "Número de aprovados: " aprovados))
      (println (str "Número de reprovados: " reprovados))
      (println (str "Maior nota: " maior-nota))
      (println (str "Menor nota: " menor-nota))
      (println (format "Média geral da turma: %.2f" media))))
  alunos)

(defn- buscar-aluno [alunos]
  (println "\n--- 4. Buscar Aluno (Extra) ---")
  (println "Digite o nome (ou parte do nome) do aluno:")
  (let [busca (clojure.string/lower-case (read-line))
        encontrados (filter #(clojure.string/includes?
                             (clojure.string/lower-case (:nome %))
                             busca)
                           alunos)]
    (if (empty? encontrados)
      (println "Nenhum aluno encontrado com esse nome.")
      (doseq [aluno encontrados]
        (let [aluno-com-status (adicionar-status aluno)]
          (println (str "\tNome: " (:nome aluno-com-status)
                        ", Nota: " (:nota aluno-com-status)
                        ", Status: " (:status aluno-com-status)))))))
  alunos)

(defn- exibir-menu []
  (println "\n=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatório de Notas")
  (println "3 - Estatísticas Gerais")
  (println "4 - Buscar Aluno (Extra)")
  (println "0 - Sair")
  (println "Escolha uma opção:"))

(defn -main [& args]
  (loop [alunos []]
    (exibir-menu)
    (let [opcao (read-line)]
      (cond
        (= opcao "1") (recur (cadastrar-alunos alunos))
        (= opcao "2") (recur (relatorio-notas alunos))
        (= opcao "3") (recur (estatisticas-gerais alunos))
        (= opcao "4") (recur (buscar-aluno alunos))
        (= opcao "0") (println "Saindo do programa...")
        :else (do
                (println "Opção inválida. Tente novamente.")
                (recur alunos))))))