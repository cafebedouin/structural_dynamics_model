% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Text Authority
 *   domain: legal_theory/constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The living constitutionalist reading of constitutional text authority
 *   holds that the Constitution's meaning evolves with social attitudes and
 *   values, drawing authority from contemporary moral principles and ancient
 *   values applied to changing circumstances. This reading instantiated Brown
 *   v. Board (1954) as a legitimate transformation of constitutional meaning
 *   without Article V amendment, recognizing that 'separate but equal' had
 *   become constitutionally impermissible through evolution in societal
 *   understanding of equality. The constraint operates as a flexible
 *   interpretive framework allowing judicial adaptation, where contemporary
 *   values gate permissible outcomes and unenumerated rights become
 *   recognizable through evolving understanding. It coordinates
 *   constitutional governance across changing social conditions but extracts
 *   from those who hold fixed-meaning commitments (originalist practitioners,
 *   democratic majorities whose enactments are overridden, formalists
 *   requiring stable meaning).
 *
 * KEY AGENTS:
 *   - contemporary_citizens_seeking_rights_recognition: Primary beneficiary (organized/biographical/constrained) — gains rights recognition through evolving interpretation
 *   - judicial_actors_exercising_evolving_interpretation: Agenda setter / beneficiary (institutional/biographical/arbitrage) — exercises interpretive authority, gains institutional legitimacy
 *   - marginalized_groups_gaining_protection_through_evolution: Beneficiary (organized/generational/constrained) — obtains constitutional protections unavailable under originalist reading
 *   - originalist_constitutional_practitioners: Victim (organized/biographical/constrained) — their interpretive methodology is displaced, professional commitments undermined
 *   - democratic_majorities_whose_enactments_are_overridden: Victim (powerful/biographical/constrained) — legislative choices invalidated by judicial evolution
 *   - legal_formalists_requiring_stable_meaning: Victim (moderate/civilizational/constrained) — rule of law values of predictability and stability eroded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.28).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.42).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal_theory/constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'ba765cf9-bfbe-4106-b339-7006c241c5c0').
narrative_ontology:cs_kernel_codification('ba765cf9-bfbe-4106-b339-7006c241c5c0', fixed_text).
narrative_ontology:cs_authority_grounding('ba765cf9-bfbe-4106-b339-7006c241c5c0', lineage).
narrative_ontology:cs_interpretation_layer_present('ba765cf9-bfbe-4106-b339-7006c241c5c0').
narrative_ontology:cs_reading_relation('ba765cf9-bfbe-4106-b339-7006c241c5c0', constitutional_text_authority__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ba765cf9-bfbe-4106-b339-7006c241c5c0', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('ba765cf9-bfbe-4106-b339-7006c241c5c0', foundational, constitutional_meaning_evolves_with_social_attitudes).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_social_attitudes, holdable).
narrative_ontology:cs_axiom_grounding('ba765cf9-bfbe-4106-b339-7006c241c5c0', constitutional_meaning_evolves_with_social_attitudes, deontological).
narrative_ontology:cs_axiom('ba765cf9-bfbe-4106-b339-7006c241c5c0', foundational, judicial_authority_to_recognize_unenumerated_rights).
narrative_ontology:cs_axiom_status(judicial_authority_to_recognize_unenumerated_rights, holdable).
narrative_ontology:cs_axiom_grounding('ba765cf9-bfbe-4106-b339-7006c241c5c0', judicial_authority_to_recognize_unenumerated_rights, deontological).
narrative_ontology:cs_axiom('ba765cf9-bfbe-4106-b339-7006c241c5c0', secondary, brown_v_board_legitimate_without_article_v).
narrative_ontology:cs_axiom_status(brown_v_board_legitimate_without_article_v, holdable).
narrative_ontology:cs_axiom_grounding('ba765cf9-bfbe-4106-b339-7006c241c5c0', brown_v_board_legitimate_without_article_v, empirically_contingent).
narrative_ontology:cs_reference_frame('ba765cf9-bfbe-4106-b339-7006c241c5c0', ratification_understanding_as_starting_point).
narrative_ontology:cs_drift_state('ba765cf9-bfbe-4106-b339-7006c241c5c0', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ba765cf9-bfbe-4106-b339-7006c241c5c0', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, contemporary_citizens_seeking_rights_recognition).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, judicial_actors_exercising_evolving_interpretation).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, marginalized_groups_gaining_protection_through_evolution).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_constitutional_practitioners).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, democratic_majorities_whose_enactments_are_overridden).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, legal_formalists_requiring_stable_meaning).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, constitutional_meaning_evolves_with_society).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_recognizable_through_evolving_understanding).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, judicial_adaptation_legitimate_without_article_v).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens and groups seeking constitutional recognition of rights not enumerated in the 1787 text (privacy, dignity, marriage equality, bodily autonomy). They benefit when courts apply evolving standards to recognize new protections. Their exit from this interpretive framework is constrained — they cannot easily 'switch' to a different constitutional methodology; their claims are structured by the living constitutionalist vocabulary itself.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, contemporary_citizens_seeking_rights_recognition, beneficiary,
    organized, biographical, constrained, national).

% Judges (especially Supreme Court justices) who exercise the authority to interpret the Constitution as an evolving document. They set the agenda by determining which social attitudes count as 'contemporary moral principles' and which ancient values apply. They benefit from expanded institutional role and legitimacy as constitutional expositors. Their exit options are arbitrage-grade: they can modulate their interpretive approach case by case, and institutional position insulates them from direct accountability.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, judicial_actors_exercising_evolving_interpretation, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, judicial_actors_exercising_evolving_interpretation, beneficiary).

% Groups historically excluded from constitutional protection (racial minorities, women, LGBTQ+ persons, religious minorities) who gain rights through evolving interpretation (Brown, Roe/Casey, Obergefell, etc.). The living constitutionalist reading is the primary vehicle for their constitutional claims. Exit is constrained: their very identity and political mobilization are constituted through the rights this reading recognizes; abandoning the reading means abandoning the constitutional grammar of their liberation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, marginalized_groups_gaining_protection_through_evolution, beneficiary,
    organized, generational, constrained, national).

% Judges, scholars, advocates, and officials committed to originalist methodology. The living constitutionalist reading's dominance in precedent and legal education displaces their interpretive framework, marginalizes their professional commitments, and requires them to practice within a methodology they consider illegitimate. Exit is constrained: their professional identity, scholarly networks, and institutional roles are fused with originalism; switching methodologies mid-career is professionally costly and intellectually fraught.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_constitutional_practitioners, payer,
    organized, biographical, constrained, national).

% Legislative majorities and voters whose policy enactments are invalidated by courts applying evolving constitutional standards (e.g., abortion restrictions, gun regulations, campaign finance laws, voting rules). They bear the cost of having democratic choices overridden by unelected judges claiming authority from 'evolving standards.' Exit is constrained: constitutional amendment (Article V) is practically unavailable; jurisdiction-stripping is constitutionally contested; the only exit is changing judicial appointments — a multi-decade project.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, democratic_majorities_whose_enactments_are_overridden, payer,
    powerful, biographical, constrained, national).

% Scholars, lawyers, and officials who view rule of law as requiring stable, predictable constitutional meaning. They pay the cost of living constitutionalism's indeterminacy: difficulty advising clients, unpredictable litigation outcomes, erosion of law's guidance function. Exit is constrained: formalism is a methodological commitment, not a job description; the legal system's operating assumptions have shifted toward living constitutionalism, making formalist practice structurally marginal.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legal_formalists_requiring_stable_meaning, payer,
    moderate, civilizational, constrained, national).

% Theorists who maintain that legal validity derives from formal sources (enactment, precedent, custom) not moral content. They are excluded from the living constitutionalist framework's internal logic — their law/morality distinction is treated as a category error by the reading. They have mobile exit: their intellectual project continues in academic discourse, comparative law, and statutory interpretation where the constraint does not directly govern.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, positivist_legal_theorists, excluded,
    organized, civilizational, mobile, national).

% Academic observers who study the contest between readings without being professionally constituted by any single one. They see the full structure: how living constitutionalism coordinates adaptation, how it extracts from originalist commitments, how theater ratio rises as 'evolution' becomes a vocabulary for preferred outcomes. Their analytical seat has no stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_law_scholars_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to adapt to changing social conditions and moral understanding without requiring Article V amendments — solving the problem of constitutional rigidity that would otherwise make the 1787 text illegitimate or inoperable in modern conditions.
% TRANSFER_FUNCTION: Moves interpretive authority and constitutional outcomes from fixed textual/historical meaning to contemporary moral judgment — transferring power from democratic majorities and originalist methodologies to judicial actors applying evolving standards, and transferring constitutional protections to groups and claims unrecognized at ratification.
% ABSENT_VOICES: Future generations who will inherit the constitutional architecture shaped by current evolving interpretations — they cannot object to rights recognized or denied today. Also absent: the ratifying generations whose understanding is displaced; their 'voice' is mediated through historical reconstruction by living constitutionalists themselves. Originalist practitioners are structurally excluded from authoritative interpretation despite organized presence.
% DISAPPEARANCE_RATIONALE: If living constitutionalism vanished overnight, the constitutional order would fundamentally rearrange: Brown v. Board, Roe/Casey, Obergefell, and the entire unenumerated rights jurisprudence would lose their doctrinal foundation; originalism would become the sole governing methodology; democratic majorities would regain legislative latitude in areas currently governed by evolving standards; the Supreme Court's institutional role would contract dramatically.
% FOUNDING_PROBLEM: The Constitution's fixed 1787 text (plus amendments) could not adequately govern a changing society without either constant formal amendment (politically impossible) or judicial adaptation. The founding problem was constitutional rigidity: how to maintain a written constitution's authority while allowing its meaning to evolve with social progress and moral learning.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists (Brennan, Dworkin, Strauss) attest the problem persists — social change outpaces amendment. Originalists (Scalia, Barrett, Bork) and democratic theorists (Schauer, Waldron) attest the problem was solved by the amendment process itself, and judicial adaptation usurps democratic authority. No consensus exists outside the benefiting parties; the contest is the constitutional politics of the last 70 years.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).
:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.28) reflects the reading's genuine coordination function — providing a mechanism for constitutional adaptation without constant formal amendment — tempered by asymmetric extraction where judicial actors and aligned groups benefit from displacing fixed-meaning commitments. Suppression (0.42) is moderate: the reading does not physically coerce but structurally suppresses alternative interpretive methodologies and democratic outcomes through judicial supremacy. Theater ratio (0.35) has risen over time as the coordination justification (adapting to social change) increasingly coexists with performative invocation of 'evolving standards' to reach preferred outcomes. Accessibility collapse (0.55) is moderate: originalist and positivist alternatives persist robustly in legal discourse and judicial practice. Resistance (0.68) is high: originalist counter-movements, judicial appointments strategy, and academic critique constitute sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (marginalized groups gaining protection), the constraint appears as a Rope — genuine coordination solving the problem of constitutional rigidity. From the victim seat (originalist practitioners, democratic majorities), it appears as a Snare — extraction of interpretive authority and legislative power under cover of 'evolution.' The engine computes this seat divergence from the structural data; the claimed_type (Tangled Rope) reflects the author's assessment that BOTH functions are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (contemporary citizens, marginalized groups, judicial actors) sit at low directionality (d ≈ 0.15–0.25): the constraint subsidizes their interests by making constitutional meaning responsive to their claims. Victims (originalist practitioners, democratic majorities, formalists) sit at high directionality (d ≈ 0.75–0.85): the constraint extracts their interpretive authority, legislative power, and rule-of-law expectations. The agenda-setter role (judicial actors) is dual-positioned: they administer the constraint and benefit from its expansion, but also bear institutional legitimacy costs when perceived as overreaching. Exit options are constrained for all non-analytical seats — constitutional interpretation is not a market with easy switching; identity_locked dynamics operate for professional communities whose identity fuses with their methodology.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional rigidity preventing adaptation to moral progress) remains live but contested. The reading's coordination function (enabling Brown, Obergefell, etc.) is real and ongoing. However, mandatrophy risk emerges as the reading's logic extends to ever-broader unenumerated rights without clear limiting principles, and as theater_ratio rises — suggesting the coordination justification increasingly covers extraction. The founding_problem_status 'contested' captures this: proponents say the problem persists; critics say the solution has become the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Does this constraint represent one reading of a contested kernel (constitutional_text_authority) rather than a free-standing constraint?',
    'This is a structural authoring fact: the constraint is explicitly instantiated as the living_constitutionalist_reading of kernel constitutional_text_authority. Sibling readings are originalist_reading and positivist_reading. Resolution is not empirical but authorial — the committer frame is declared by the generation prompt.',
    'Confirms this story is one member of a constraint family. Classification, ε, and beneficiary/victim structure are reading-indexed; other readings produce distinct constraints with their own metrics. The engine must not average or reconcile across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This constraint is a kernel reading, not a free-standing constraint').

omega_variable(
    living_vs_originalist_foreclosure,
    'Does the living constitutionalist reading foreclose the originalist reading within a single commitment framework, or do they coexist as live positions?',
    'Assess logical structure: living constitutionalism holds that constitutional meaning *must* evolve with social attitudes; originalism holds that meaning *is fixed* at ratification. A single legal framework cannot simultaneously hold both as operative premises — one governs or the other does. However, different parties in a polity can hold different readings simultaneously (judicial coalitions, scholarly camps, public opinion).',
    'If forecloses: the readings are mutually exclusive within any single authoritative framework (e.g., a court adopting living constitutionalism logically rejects originalism as a governing methodology). If coexists_with: both remain live positions in public discourse and judicial practice, held by different actors. This determines the reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_vs_originalist_foreclosure, conceptual, 'Structural relationship between living constitutionalist and originalist readings').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the measured extractiveness (0.28) primarily the cost of genuine coordination (stabilizing constitutional meaning amid social change) or does it contain asymmetric extraction (judicial aggrandizement, elite preference imposition)?',
    'Compare outcomes: when living constitutionalist interpretation expands protections for marginalized groups (Brown v. Board, Obergefell), beneficiaries are identifiable and coordination function is served. When it strikes down democratic enactments without clear textual basis, critics identify extraction. The boundary is contested — the omega documents the ambiguity.',
    'If primarily coordination cost: Tangled Rope classification holds. If substantial asymmetric extraction: Snare classification may emerge for specific payer seats (democratic majorities, originalist practitioners). The engine computes per-seat χ from structural data; this omega flags the interpretive ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether measured extraction is coordination cost or asymmetric extraction').

omega_variable(
    unenumerated_rights_scope,
    'How far does the living constitutionalist reading''s recognition of unenumerated rights extend — is there a structural limit, or does the reading''s logic permit unbounded judicial recognition of new rights?',
    'Track doctrinal development: substantive due process, privacy, dignity, equality jurisprudence. If the reading contains internal limiting principles (tradition, history, reasoned judgment), extraction is bounded. If the logic is open-ended, extraction potential is unbounded and suppression of democratic alternatives increases.',
    'Unbounded recognition → higher effective extraction for payer seats, higher suppression of democratic alternatives, potential drift toward Snare. Bounded recognition → Tangled Rope stable. This is a structural question about the reading''s internal logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_scope, conceptual, 'Structural limits on unenumerated rights recognition within living constitutionalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 1937, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1937, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(cons_tr_t1954, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(cons_tr_t1965, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(cons_tr_t1973, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(cons_tr_t1992, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1992, 0.32).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(cons_tr_t2022, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2022, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t1937, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement(cons_be_t1954, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(cons_be_t1965, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(cons_be_t1973, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1973, 0.28).
narrative_ontology:measurement(cons_be_t1992, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1992, 0.26).
narrative_ontology:measurement(cons_be_t2015, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(cons_be_t2022, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2022, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1937, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(cons_su_t1954, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(cons_su_t1965, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(cons_su_t1973, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(cons_su_t1992, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1992, 0.42).
narrative_ontology:measurement(cons_su_t2015, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(cons_su_t2022, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2022, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__living_constitutionalist_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, judicial_review_legitimacy).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_doctrine).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, stare_decisis_in_constitutional_law).

% DUAL FORMULATION NOTE:
% Constitutional text authority decomposes into three readings with distinct ε and beneficiary/victim structures. Living constitutionalist reading (this story): ε=0.28, coordinates adaptation but extracts from fixed-meaning commitments. Originalist reading: ε≈0.15, coordinates stability but extracts from evolving-rights claimants. Positivist reading: ε≈0.1, coordinates formal validity but extracts from moral-reading proponents. The family is linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, institutional, 0.25).
constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, organized, 0.2).
constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, powerful, 0.8).
constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
