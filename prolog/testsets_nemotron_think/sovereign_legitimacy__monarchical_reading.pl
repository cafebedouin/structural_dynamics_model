% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy: Divine Right and Bloodline Succession
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint story models the monarchical reading of sovereign
 *   legitimacy: authority flows downward from a hereditary sovereign whose
 *   right derives from divine sanction, tradition, and bloodline continuity.
 *   The arrangement presents itself as natural law (mountain-like) but
 *   structurally operates as a snare — extracting compliance and surplus from
 *   a trapped subject population to benefit a hereditary ruling class and its
 *   religious legitimators. Active enforcement maintains the exclusion of
 *   alternative legitimacy claims (republican, contractual, meritocratic).
 *   The ritual theater of coronation, anointing, and court ceremony serves as
 *   both coordination signal (succession clarity) and extraction mask (divine
 *   theater obscuring material transfer).
 *
 * KEY AGENTS:
 *   - hereditary_monarch: Primary agenda setter (institutional/identity_locked) — embodies the constraint, cannot exit without dissolving it
 *   - aristocratic_hierarchy: Primary beneficiary (organized/constrained) — holds titles and lands contingent on constraint persistence
 *   - religious_establishment: Beneficiary and secondary agenda setter (institutional/constrained) — provides divine sanction in exchange for privileges
 *   - subject_population: Primary victim (powerless/trapped) — bears extraction with near-zero exit
 *   - excluded_political_actors: Excluded seat (moderate/constrained) — would claim authority share if constraint permitted
 *   - analytical_observer: Observer seat (analytical/analytical) — sees full structure without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.75).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.82).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy: Divine Right and Bloodline Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '25ba0bc9-733e-4296-b6d0-3cef2caeb6e6').
narrative_ontology:cs_kernel_codification('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', fixed_text).
narrative_ontology:cs_authority_grounding('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', lineage).
narrative_ontology:cs_interpretation_layer_present('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6').
narrative_ontology:cs_reading_relation('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', foundational, divine_right_of_kings).
narrative_ontology:cs_axiom_status(divine_right_of_kings, holdable).
narrative_ontology:cs_axiom_grounding('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', divine_right_of_kings, theological).
narrative_ontology:cs_axiom('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', foundational, bloodline_continuity_as_legitimacy).
narrative_ontology:cs_axiom_status(bloodline_continuity_as_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', bloodline_continuity_as_legitimacy, conventional).
narrative_ontology:cs_axiom('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', secondary, subjects_excluded_from_sovereign_authority).
narrative_ontology:cs_axiom_status(subjects_excluded_from_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', subjects_excluded_from_sovereign_authority, theological).
narrative_ontology:cs_reference_frame('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', divinely_ordained_hierarchy).
narrative_ontology:cs_drift_state('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', modern_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25ba0bc9-733e-4296-b6d0-3cef2caeb6e6', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, court_nobility).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, religious_establishment).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subject_population).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_political_actors).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_of_kings).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_as_legitimacy).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, traditional_hierarchy_as_natural_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodies the sovereign authority; legitimacy derives entirely from bloodline succession and divine sanction. Cannot exit the role without dissolving the constraint itself. Administers succession, appoints nobility, commands ritual validation. Collects obedience, revenue, and symbolic capital from the subject population.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_monarch, agenda_setter,
    institutional, generational, identity_locked, national).

% Holds hereditary titles, land, and administrative offices granted by the monarch. Their status and material privileges depend on the monarchical constraint's persistence. Exit means renunciation of title and privilege — possible but socially and economically costly. They enforce the constraint locally through manorial courts, patronage, and military service.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    organized, generational, constrained, national).

% Proximate to the sovereign; converts ceremonial access into political influence and economic rent. Their position is contingent on monarch's favor but the constraint's structure guarantees their class interest. Exit is constrained by loss of court office and the patronage networks it controls.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, court_nobility, beneficiary,
    powerful, biographical, constrained, national).

% Provides divine sanction through coronation rites, sacramental kingship theology, and obedience preaching. Receives legal privileges, tithe rights, and moral authority in return. The constraint's theological grounding is their institutional asset. Exit would mean losing established church status and the monarch's protection.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, religious_establishment, beneficiary,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, religious_establishment, agenda_setter).

% Bears taxation, conscription, labor obligations, and legal subjection without political participation. Legitimacy discourse frames their subjection as natural and divinely ordered. Exit options are near-zero: migration is restricted, rebellion is treason, and internalized legitimacy narratives make resistance cognitively difficult. The constraint extracts surplus labor and compliance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subject_population, payer,
    powerless, biographical, trapped, national).

% Merchant elites, intellectuals, military officers, and local magistrates who possess administrative competence or economic power but are barred from sovereign authority. They would claim a share of legitimacy if the constraint allowed it. Their exclusion is structural — the constraint defines authority as inherited, not earned. Exit means emigration or quiet acquiescence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_political_actors, excluded,
    moderate, biographical, constrained, national).

% Comparative political theorist or historian examining the constraint's structure across regimes and epochs. Sees the full beneficiary/victim asymmetry, the ritual-theater of legitimacy, and the historical contingency of divine-right claims. No material stake in the constraint's persistence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic succession mechanism that prevents interregnum chaos and elite fragmentation upon a ruler's death. The bloodline rule settles 'who rules next' without civil war among claimants — a genuine coordination problem in pre-bureaucratic polities.
% TRANSFER_FUNCTION: Moves surplus (tax revenue, labor service, military levies, symbolic deference) from the subject population and excluded political actors to the hereditary monarch, aristocratic hierarchy, court nobility, and religious establishment. The transfer is justified as the cost of order and divine order.
% ABSENT_VOICES: The subject population — particularly peasantry, urban poor, and colonized peoples — would object to their exclusion from authority if they could speak without reprisal. They are structurally silenced by the constraint's definition of political agency as inheritable status. Also absent: future generations bound by succession rules they never consented to.
% DISAPPEARANCE_RATIONALE: If the monarchical legitimacy constraint vanished overnight, succession would become contested, aristocratic titles would lose legal force, religious establishment privileges would be challenged, and the subject population would demand participatory governance. The polity would reorganize around a new legitimacy kernel (republican, constitutional hybrid, or warlord fragmentation).
% FOUNDING_PROBLEM: Pre-modern polities faced chronic succession crises: elective monarchies produced factional warfare, usurpation was normalized, and interregna invited invasion. The hereditary divine-right formula solved this by making succession automatic, sacralized, and undisputable — at the cost of freezing political agency in bloodline.
% FOUNDING_PROBLEM_CORROBORATION: The succession-crisis problem is historically dead in bureaucratic states with constitutional succession rules. Corroborated by political historians (e.g., Finer, Skinner) and comparative constitutional scholars outside the monarchical tradition. The monarchical reading's own apologists now argue the constraint serves symbolic unity, not succession stability — a shifted function, not the founding one.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the subject population surrenders surplus and agency without reciprocity; the coordination benefit (succession stability) is real but disproportionately captured by the beneficiary class. Suppression (0.82) is very high — alternative legitimacy sources (popular consent, election, merit) are not merely discouraged but defined as illegitimate/treasonous. Theater ratio (0.42) reflects that coronation rites, royal progresses, and court ritual are partly functional (coordination signaling) but increasingly performative as bureaucratic administration displaces personal rule. Accessibility collapse (0.73) is high because the constraint defines the political imagination: subjects struggle to conceive authority otherwise. Resistance (0.51) is moderate — peasant revolts, noble factionalism, and intellectual dissent exist but are structurally contained.
 *
 * PERSPECTIVAL GAP:
 *   The hereditary_monarch and religious_establishment seats experience the constraint as legitimate coordination (low effective extraction, high coordination value). The subject_population experiences it as pure extraction with no exit (high effective extraction). The aristocratic_hierarchy and court_nobility sit in between — they benefit materially but are constrained by the monarch's supreme authority. The analytical_observer sees the full asymmetry. The engine computes this divergence from the structural power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: hereditary_monarch, aristocratic_hierarchy, court_nobility, religious_establishment — all collect material or symbolic rents from the constraint. Victims declared: subject_population, excluded_political_actors — both bear costs without authority participation. The monarch is identity_locked (cannot exit the role), making d approach 1.0 for the constraint's extraction logic despite being a beneficiary — the monarch is both extractor and extracted-from (by the constraint's own logic). Subjects are trapped (exit_options: trapped) — d near 1.0. Excluded actors are constrained — d ~0.7. Religious establishment is constrained by mutual dependence — d ~0.3.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (succession crisis prevention) is dead in modern bureaucratic states, yet the constraint persists in ceremonial monarchies and survives in authoritarian personalist regimes that mimic monarchical legitimation. The constraint has undergone mandatrophy: its original coordination function atrophied, but the extraction structure (surplus transfer to a hereditary/court elite) persists, now justified by symbolic unity or stability narratives. The theater ratio rise from 0.35 to 0.42 tracks this — more performance, less functional coordination. The constraint is not a piton because the beneficiary class (monarch, aristocracy, court) still actively maintains it and captures concentrated rents; it is a snare with a dead founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this monarchical reading''s structural classification change if the kernel''s other readings (republican, constitutional_hybrid) are simultaneously live in the same polity?',
    'Compare constraint classifications across a polity with competing legitimacy claims (e.g., UK: monarchical ceremonial + republican-democratic political). If the monarchical constraint''s extractiveness drops when republican constraints coexist, the monarchical reading''s ε is context-dependent on being the sole legitimacy claim.',
    'If ε is context-dependent, the ε-invariance principle requires decomposing the monarchical reading into ''sole-claim'' and ''coexisting-claim'' variants as separate constraints. If invariant, the monarchical constraint''s extraction persists regardless of competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the monarchical reading''s extractiveness depends on being the exclusive legitimacy claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.82) primarily structural (legal bans on republican speech, censorship, treason laws) or internalized (subjects believe monarchical authority is natural/divine, making resistance cognitively unavailable)?',
    'Historical analysis of post-monarchy transitions: if republican movements emerge rapidly after constraint collapse, suppression was structural. If legitimacy narratives persist generations after formal abolition, internalized suppression was significant.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — subjects carry the suppression with them. This would increase χ for the subject_population seat beyond what structural suppression alone predicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in monarchical legitimacy').

omega_variable(
    divine_sanction_natural_law_ambiguity,
    'Does the constraint''s claim to divine sanction and natural law reflect a genuine Mountain-like natural limit (God''s will), or is it a constructed Snare that benefits identifiable agents?',
    'Theological-historical analysis: if the divine-right doctrine was formulated ex post to justify existing power (e.g., Bodin, Filmer responding to specific succession crises), it is constructed. If the doctrine precedes and constrains power (e.g., ancient sacral kingship where the king is sacrificial victim), it may have Mountain-like features.',
    'If constructed, the constraint is a false_summit_mountain candidate — declared Mountain by beneficiaries, computed Snare by engine. If genuine sacral constraint with the king as first victim, the beneficiary/victim structure inverts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_sanction_natural_law_ambiguity, conceptual, 'Natural-law vs. constructed ambiguity of divine-right legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 1000, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1000, sovereign_legitimacy__monarchical_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(sove_tr_t1200, sovereign_legitimacy__monarchical_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(sove_tr_t1400, sovereign_legitimacy__monarchical_reading, theater_ratio, 1400, 0.41).
narrative_ontology:measurement(sove_tr_t1600, sovereign_legitimacy__monarchical_reading, theater_ratio, 1600, 0.44).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__monarchical_reading, theater_ratio, 1800, 0.46).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__monarchical_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(sove_be_t1000, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement(sove_be_t1200, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1200, 0.71).
narrative_ontology:measurement(sove_be_t1400, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1400, 0.73).
narrative_ontology:measurement(sove_be_t1600, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1600, 0.76).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1800, 0.78).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__monarchical_reading, base_extractiveness, 2000, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1000, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(sove_su_t1200, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1200, 0.78).
narrative_ontology:measurement(sove_su_t1400, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1400, 0.81).
narrative_ontology:measurement(sove_su_t1600, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1600, 0.84).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1800, 0.86).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__monarchical_reading, suppression_requirement, 2000, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.1).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This monarchical_reading, the republican_reading, and the constitutional_hybrid_reading form a constraint family decomposing the sovereign_legitimacy kernel. The monarchical reading asserts exclusive divine-right authority (high ε, high suppression). The republican reading asserts exclusive popular sovereignty (different beneficiary/victim structure). The hybrid reading splits authority domains (ceremonial vs political) creating a scaffold-like transitional structure. All three share the kernel_id sovereign_legitimacy but instantiate distinct constraints with distinct ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, institutional, 0.15).
constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
