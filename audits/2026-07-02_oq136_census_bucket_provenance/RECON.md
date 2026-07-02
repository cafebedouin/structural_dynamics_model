# OQ-136 RECON — re-witnessed counts at execution time

- git rev: `0ba48b4c6dc91a5b27000e2f2ea694f06808b103` (dirty=True) — the audit's own stamp; the pipeline
  manifest is NOT cited (it may be stale relative to this run).
- corpus loaded: n=119 (corpus_loader count from the extract run)
- bucket counts: {"q6_unmeasured": 26, "q6_signature_unknown": 16, "extraction_unnameable": 3, "no_agent_seats": 26, "manufactured_consensus_candidate": 9}
- powered (n>=8): ['q6_unmeasured', 'q6_signature_unknown', 'no_agent_seats', 'manufactured_consensus_candidate']
- unpowered: ['extraction_unnameable']
- provenance authored: 110/119; missing: ['actinide_replenishment_mechanism_contradictions', 'digital_money_legitimacy_contradictions', 'generality_standard_contradictions', 'knowledge_legitimacy_biomedicine_contradictions', 'learning_difficulty_substrate_contradictions', 'moral_causation_locus_contradictions', 'performance_legitimacy_contradictions', 'polaris_document_status_contradictions', 'visual_evidentiary_authority_contradictions']
- json twin crosscheck: 110 checked, 0 mismatches, 0 twins missing
- extractor controls fired: drop-one [[q6] sigma members (118) != n_corpus (119)]; duplicate [[q6] non-exactly-one cids: ['ability_ceiling_reading']]
- statistic controls: planted-cluster flagged=True (p=2.824e-07); uniform-draw flagged=False (p=1)
