(ns atividade.exc)

(def A "A")
(def B "B")
(def C "C")
(def D "D")
(def F "F")
(def limite 60.0)

(defn -main []
  (print "informe a quantidade alunos") 

  (let [alunos (Integer/parseInt (read-line))]

    (loop [alunoAtual 1
           soma-notas 0.0
           aprovados-count 0]

      (if (> alunoAtual alunos)

        (let [media-turma (/ soma-notas (double alunos))
              desempenho-msg (if (>= media-turma 80.0)
                               "Turma excelente!"
                               (if (>= media-turma 60.0)
                                 "Bom desempenho!"
                                 "É necessário melhorar!"))]

          (println "\nMédia da turma:" media-turma)
          (println "Aprovados:" aprovados-count)
          (println "Desempenho geral:" desempenho-msg))

        (do
          (println "\nNome do aluno" alunoAtual) 
          (let [nome (read-line)]
            (print "Nota: ") (flush)

            (let [nota (Double/parseDouble (read-line))
                  conc (cond
                             (>= nota 90) A
                             (>= nota 80) B
                             (>= nota 70) C
                             (>= nota 60) D
                             :else F)
                  novo-aprovados-count (if (>= nota limite)
                                         (inc aprovados-count)
                                         aprovados-count)]

              (println nome "-" "Conceito:" conc)

              (recur (inc alunoAtual)
                     (+ soma-notas nota)
                     novo-aprovados-count))))))))