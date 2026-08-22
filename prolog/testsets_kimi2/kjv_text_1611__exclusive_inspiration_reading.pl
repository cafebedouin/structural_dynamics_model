% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV-Only Exclusive Inspiration Claim
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This constraint instantiates the exclusive_inspiration_reading of kernel
 *   kjv_text_1611: the claim that the 1611 King James Version is the
 *   exclusively inspired, inerrant English Bible and that all other
 *   translations are corrupted or inferior. The reading creates a gatekeeping
 *   structure in which KJV-Only institutional leadership becomes the sole
 *   arbiter of legitimate scripture, modern translations and their users
 *   enter the victim set, and textual scholarship is structurally excluded
 *   from the legitimacy conversation. The claim is presented as fidelity to
 *   God's preserved word; the authored metrics describe a heavily extractive,
 *   actively enforced constraint whose persistence depends on suppressing
 *   alternatives and excluding empirical textual criticism.
 *
 * KEY AGENTS:
 *   - kjv_only_institutions: Primary beneficiary and agenda-setter (institutional/identity_locked/global) â sets doctrine, enforces exclusivity, collects authority and revenue
 *   - modern_translation_communities: Primary target (moderate/constrained/global) â bear exclusion, loss of legitimacy, and restricted access within KJV-Only networks
 *   - biblical_scholarship_community: Excluded analytical seat (powerful/analytical/global) â possesses counter-evidence but is kept out of the legitimacy conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.82).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.88).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Claim").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious_studies/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, 'c08caab1-b41d-43cc-9370-b77ca4d6e622').
narrative_ontology:cs_kernel_codification('c08caab1-b41d-43cc-9370-b77ca4d6e622', fixed_text).
narrative_ontology:cs_authority_grounding('c08caab1-b41d-43cc-9370-b77ca4d6e622', lineage).
narrative_ontology:cs_interpretation_layer_present('c08caab1-b41d-43cc-9370-b77ca4d6e622').
narrative_ontology:cs_reading_relation('c08caab1-b41d-43cc-9370-b77ca4d6e622', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('c08caab1-b41d-43cc-9370-b77ca4d6e622', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('c08caab1-b41d-43cc-9370-b77ca4d6e622', foundational, kjv_exclusive_verbal_plenary_inspiration).
narrative_ontology:cs_axiom_status(kjv_exclusive_verbal_plenary_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('c08caab1-b41d-43cc-9370-b77ca4d6e622', kjv_exclusive_verbal_plenary_inspiration, theological).
narrative_ontology:cs_axiom('c08caab1-b41d-43cc-9370-b77ca4d6e622', foundational, english_preservation_requires_kjv_supremacy).
narrative_ontology:cs_axiom_status(english_preservation_requires_kjv_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c08caab1-b41d-43cc-9370-b77ca4d6e622', english_preservation_requires_kjv_supremacy, theological).
narrative_ontology:cs_reference_frame('c08caab1-b41d-43cc-9370-b77ca4d6e622', kjv_inerrant_english_preserved_text).
narrative_ontology:cs_drift_state('c08caab1-b41d-43cc-9370-b77ca4d6e622', contemporary_textual_criticism_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c08caab1-b41d-43cc-9370-b77ca4d6e622', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_communities).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textual_preservationism).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, byzantine_priority_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the doctrine that the 1611 King James Version is the exclusively inspired, inerrant English Bible. Control publishing houses, conference networks, Bible colleges, and missionary agencies that enforce the exclusivity claim. Derive institutional authority, donations, and market share from being the sole arbiters of legitimate scripture. Abandoning the claim would dissolve their boundary identity and authority.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions, beneficiary).

% Congregations, missionaries, and individual believers who use modern translations such as the NIV, ESV, or NASB. In KJV-Only environments they are labeled as using corrupted texts, barred from pulpits, denied missionary support, and treated as doctrinally suspect. Exit requires either accepting the KJV-Only frame or leaving the network entirely.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_communities, payer,
    moderate, biographical, constrained, global).

% Textual critics, translation committees, and academic theologians whose manuscript work and linguistic scholarship directly contradict the exclusive-inspiration claim. They are not engaged as interlocutors in KJV-Only discourse; instead they are dismissed as unbelievers or textual revisionists. Their expertise is structurally excluded from the legitimacy conversation.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, biblical_scholarship_community, excluded,
    powerful, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutions).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, fixed, and unambiguous English biblical text around which a religious community can coordinate doctrine, worship, education, and missionary identity without permitting revision or pluralism.
% TRANSFER_FUNCTION: Moves textual authority from the manuscript tradition and scholarly consensus to KJV-Only institutional leadership; moves legitimacy and community standing away from modern translations and their users toward the exclusive KJV text.
% ABSENT_VOICES: Biblical textual scholars, modern translation committees, mainstream evangelical denominations, and ecumenical church bodies are excluded from the legitimacy conversation; they would argue for translation plurality based on manuscript evidence and linguistic updating but are dismissed as apostate or corrupt.
% DISAPPEARANCE_RATIONALE: If the exclusive inspiration claim vanished, KJV-Only institutions would lose their primary boundary marker and authority source. Their colleges, publishing houses, and missionary networks would reintegrate into broader evangelicalism; modern translations would enter KJV-Only pulpits; and the leadership's distinctive power to adjudicate true scripture would dissolve.
% FOUNDING_PROBLEM: The perceived threat of modernist biblical criticism and modern-language translations to doctrinal certainty; the absence of a recognized perfect English Bible to unify fundamentalist resistance against liberal theology and textual revision.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of American fundamentalism and textual scholars attest the movement arose in specific 20th-century controversies and that the founding problem of vernacular Bible access is long solved. No outside corroboration exists for the exclusivity claim's founding problem; KJV-Only institutions alone assert the problem remains live.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the exclusivity claim transfers textual authority from the broad manuscript tradition to a single institutional gate. Suppression is higher (0.88) because the constraint's persistence depends on actively excluding modern translations, textual scholars, and alternative readings as corrupt â not on participant preference. Theater is moderate (0.40): the KJV's genuine literary and historical stature provides real cover, but a growing share of KJV-Only activity defends an exclusivity claim that scholarship treats as settled. Accessibility collapse is substantial (0.68) for those inside the community, where alternatives are treated as spiritually dangerous. Resistance is significant (0.72) from mainstream evangelicalism and biblical scholarship. The temporal series trace the movement's institutional maturation from mid-century fundamentalist controversy to a global gatekeeping network with escalating enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as sacred stewardship of God's preserved word; the payer seat experiences the same structure as authoritarian gatekeeping that suppresses legitimate scholarship and alternative translations. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The kjv_only_institutions are declared beneficiaries and agenda-setters (low d, subsidized by the constraint's authority monopoly). The modern_translation_communities are declared victims and payers (high d, extraction amplified by constrained exit and national-to-global scope). The biblical_scholarship_community is excluded rather than targeted; their exclusion is the enforcement perimeter that makes the extraction possible. Directionality derivation should place the institutions near the beneficiary pole and translation communities near the target pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy interview prevents mislabeling this as a Rope or Scaffold of tradition. The founding problem â a 20th-century fundamentalist reaction to modernism â is contested, with independent historians attesting it is dead. If this were genuine coordination, we would expect low suppression, low theater, and a live founding problem. Instead, suppression is high (0.88), theater is rising (0.40), and the founding problem lacks outside corroboration. This mismatch flags the constraint as a snare using a coordination narrative (preservation of scripture) as cover for authority extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kjv_kernel_reading_position,
    'Does this constraint represent a genuine feature of the KJV textual tradition, or a constructed gatekeeping mechanism layered onto the KJV kernel by a 20th-century authority structure?',
    'Comparative structural analysis of the sibling readings; examination of whether the exclusivity claim can be separated from the KJV translation itself without collapsing the kernel.',
    'If the exclusivity is separable from the kernel, the constraint is a snare of authority extraction; if inseparable, it would be reclassified as a mountain-like feature of the tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kjv_kernel_reading_position, conceptual, 'Natural-law versus constructed ambiguity for the exclusivity claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional exclusion from pulpits, colleges, and missions) or internalized (theological conviction that modern translations are spiritually dangerous)?',
    'Post-exit trajectory study of former KJV-Only adherents: if fear of modern translations persists after institutional departure, reclassify suppression as partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds structural measures â the target carries the suppression after exit, indicating cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    extraction_nature_ambiguity,
    'Does the extraction operate primarily through financial transfer (book sales, tithes, institutional donations) or through epistemic capture (monopoly on divine authority and identity gatekeeping)?',
    'Financial audit of KJV-Only institutions versus mainstream Christian publishing; ethnographic study of authority claims and identity fusion in KJV-Only communities.',
    'If financial, directionality maps clearly to institutional beneficiaries; if purely epistemic, gain_flow may be diffuse but extraction remains real through identity-locked costs borne by adherents and excluded scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_nature_ambiguity, empirical, 'Financial versus epistemic extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(kjv__tr_t8, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(kjv__tr_t16, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(kjv__tr_t24, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(kjv__tr_t32, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(kjv__be_t8, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(kjv__be_t16, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(kjv__be_t24, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(kjv__be_t32, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(kjv__su_t8, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(kjv__su_t16, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(kjv__su_t24, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(kjv__su_t32, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
