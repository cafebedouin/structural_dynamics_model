% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus (Constitutional Conformity Doctrine)
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This story authors the reformist-egalitarian reading of the Vedic-dharmic
 *   corpus: the claim that hierarchical textual passages are historical
 *   accretion rather than scriptural essence, and that constitutional
 *   equality principles must govern textual interpretation, with rational
 *   critique superseding inherited traditional authority. This is one of
 *   three structurally distinct readings of a single contested kernel (the
 *   corpus and its interpretive authority). The hereditary_monopoly_reading
 *   treats varna hierarchy as divinely ordained textual content; the
 *   bhakti_devotional_reading treats caste as irrelevant to spiritual access
 *   via devotion, bypassing the equality-versus-hierarchy debate entirely.
 *   This story generates ONLY the reformist reading as its own constraint,
 *   with its own epsilon, beneficiaries, and victims — it does not average
 *   across readings or describe the contest inside its own classification.
 *
 * KEY AGENTS:
 *   - dalit_rights_movements: Primary beneficiary (organized/constrained) — gains legal standing and access through the reading's adoption
 *   - constitutional_courts: Agenda-setter and secondary beneficiary (institutional/analytical) — administers the interpretive standard and gains authority from its application
 *   - orthodox_brahmin_institutions: Primary target (organized/constrained) — loses exclusive interpretive and ritual authority
 *   - traditional_temple_authorities: Secondary target (moderate/constrained) — loses local gatekeeping revenue and control
 *   - textual_traditionalist_scholars: Excluded voice (moderate/trapped) — philological objections are not engaged as scholarship in the forums that matter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.4).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus (Constitutional Conformity Doctrine)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '49416989-67f4-4818-9f53-73234a45ef21').
narrative_ontology:cs_kernel_codification('49416989-67f4-4818-9f53-73234a45ef21', fixed_text).
narrative_ontology:cs_authority_grounding('49416989-67f4-4818-9f53-73234a45ef21', extraction).
narrative_ontology:cs_interpretation_layer_present('49416989-67f4-4818-9f53-73234a45ef21').
narrative_ontology:cs_reading_relation('49416989-67f4-4818-9f53-73234a45ef21', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('49416989-67f4-4818-9f53-73234a45ef21', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('49416989-67f4-4818-9f53-73234a45ef21', foundational, constitutional_equality_binds_textual_interpretation).
narrative_ontology:cs_axiom_status(constitutional_equality_binds_textual_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('49416989-67f4-4818-9f53-73234a45ef21', constitutional_equality_binds_textual_interpretation, conventional).
narrative_ontology:cs_axiom('49416989-67f4-4818-9f53-73234a45ef21', foundational, hierarchical_passages_are_historical_accretion_not_essence).
narrative_ontology:cs_axiom_status(hierarchical_passages_are_historical_accretion_not_essence, holdable).
narrative_ontology:cs_axiom_grounding('49416989-67f4-4818-9f53-73234a45ef21', hierarchical_passages_are_historical_accretion_not_essence, empirically_contingent).
narrative_ontology:cs_reference_frame('49416989-67f4-4818-9f53-73234a45ef21', pre_constitutional_customary_hierarchy).
narrative_ontology:cs_drift_state('49416989-67f4-4818-9f53-73234a45ef21', post_independence_constitutional_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('49416989-67f4-4818-9f53-73234a45ef21', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_temple_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, textual_meaning_is_historically_contingent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the reformist reading as a lever to contest temple entry restrictions, personal law provisions, and social exclusion practices justified by appeal to scriptural authority. Gain standing in courts and public discourse when the reading is adopted, but remain dependent on state institutions actually enforcing the equality principle rather than deferring to traditional custom.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements, beneficiary,
    organized, generational, constrained, national).

% Courts, legislatures, and reform-oriented religious bodies that adjudicate disputes by treating constitutional equality as the interpretive frame through which scripture must be read. They administer the doctrine, deciding which textual claims survive contact with equality jurisprudence, and could in principle revise the interpretive standard but have institutional incentive to hold it stable.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Rule on temple-entry, personal-law, and anti-discrimination cases by subordinating scriptural claims to constitutional equality provisions. Their authority is enhanced each time they successfully assert interpretive supremacy over religious tradition, giving them an institutional stake in the reading's continued dominance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts, beneficiary).

% Lose exclusive interpretive and ritual authority when courts and reform movements recharacterize hierarchical textual passages as historical accretion rather than binding prescription. Cannot simply exit the framework because the same state apparatus that enforces the reformist reading also administers temple registration, funding, and legal recognition of religious institutions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions, payer,
    organized, generational, constrained, national).

% Local priestly and administrative structures whose customary control over entry, ritual sequence, and lineage-based office is overridden by court orders grounded in the reformist reading. They experience direct loss of authority and revenue from ritual gatekeeping but have limited capacity to contest enforcement once courts have ruled.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_temple_authorities, payer,
    moderate, biographical, constrained, regional).

% Ordinary worshippers across caste positions who gain expanded access to temples and rites under the reformist reading, but who also experience disruption of familiar ritual practice and community structure when courts intervene. Their experience of the reading depends heavily on which side of the prior hierarchy they occupied.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners, payer).

% Scholars who hold that the hierarchical passages are original scriptural content, not accretion, are largely excluded from the legal and constitutional forums where the reformist reading is authoritative. Their philological objections are treated as theological special-pleading rather than engaged as textual-historical claims requiring adjudication.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, textual_traditionalist_scholars, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, publicly defensible standard for adjudicating disputes between traditional religious practice and modern legal equality norms, allowing courts and legislatures to resolve caste-based exclusion claims without directly litigating theology.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority over the corpus from hereditary ritual specialists to constitutional courts and reform-aligned legal institutions, and moves practical access to religious spaces and rites from higher-caste gatekeepers toward previously excluded groups.
% ABSENT_VOICES: Textual traditionalist scholars who hold the hierarchical passages are original rather than accreted are largely absent from the courts and legislative bodies that adopt the reformist reading; their philological case is treated as a religious objection rather than a scholarly one requiring independent evaluation.
% DISAPPEARANCE_RATIONALE: If the reformist reading disappeared as an interpretive standard, courts would lose their current basis for overriding customary temple-entry and personal-law restrictions; orthodox institutions would recover uncontested control over ritual gatekeeping, and Dalit movements would lose a primary legal lever for contesting exclusion — the practical landscape of access and authority would shift substantially.
% FOUNDING_PROBLEM: Colonial-era and post-independence reform movements confronted a scriptural corpus whose hierarchical passages were being used to justify caste exclusion in temples, personal law, and public life, in direct tension with the newly adopted constitutional commitment to equality.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights organizations and constitutional law scholars attest the founding problem remains live — exclusionary practice persists and requires continued doctrinal enforcement. Orthodox institutions and some independent historians of religion attest the problem was substantially addressed by mid-20th-century legal reform and that continued expansive application now functions primarily to extend court and reform-institution authority over religious administration rather than to redress active exclusion.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 — moderate, not high — because the reading redistributes interpretive authority and practical access rather than extracting material resources in the manner of a pure economic snare; the transfer is institutional and status-based. Suppression sits at a declining-then-flat 0.40-0.50: enforcement was more acute during the early court interventions establishing the doctrine (temple-entry cases, personal-law reform) and has settled into steadier, less contested application as the doctrine gained institutional embeddedness — normalization, not escalation. Theater ratio rises modestly (0.12 to 0.30) as some enforcement activity shifts toward symbolic vindication of constitutional supremacy rather than addressing residual concrete exclusion, without dominating the function.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the reading is a rope: it solves a genuine coordination problem (reconciling scripture with constitutional commitments) with net positive effect for previously excluded groups. From the orthodox institutional seats, the same structure is an enforced transfer of interpretive authority backed by state power — closer to extraction. The engine computes these divergent seat-level readings from the declared power/exit structure; the claimed_type of tangled_rope is authored because both a genuine coordination function (resolving a real doctrinal-constitutional conflict) and asymmetric extraction (orthodox institutions bear concentrated losses, enforced by active state and judicial machinery) are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit rights movements and reform-aligned legal institutions sit near the beneficiary end: the reading directly expands their standing and access. Constitutional courts benefit indirectly by having their interpretive supremacy vindicated each time the doctrine is applied — a genuine but secondary beneficiary position, hence the dual role. Orthodox Brahmin institutions and temple authorities sit near the target end: they lose exclusive control over interpretation and ritual gatekeeping through the same mechanism that redresses exclusion, and their exit options are constrained because the same state apparatus enforcing the doctrine also controls institutional recognition and funding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — scriptural hierarchy justifying caste exclusion against a constitutional equality commitment — remains genuinely live in many contexts (temple access disputes, personal law) but is contested as to whether continued expansive application now primarily extends court and reform-institution authority beyond the original exclusion it targeted. This is authored as contested rather than resolved: declaring it flatly 'dead' would ignore ongoing exclusionary practice; declaring it flatly 'live' would ignore the institutional-authority-accretion pattern visible in the theater_ratio trend.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accretion_vs_essence_historicity,
    'Is the hierarchical content of the corpus genuinely a later historical accretion onto an egalitarian or hierarchy-neutral core, or is it original content that reformist historiography has recharacterized to align with constitutional commitments?',
    'Comparative philological and textual-historical dating of manuscript strata, cross-referenced against independent (non-reform-affiliated and non-orthodox-affiliated) historical linguistics scholarship.',
    'If the hierarchical content is demonstrably later accretion, the reformist reading''s core historical claim is vindicated and its interpretive authority strengthens; if the content is original, the reading''s legitimacy rests more heavily on constitutional supremacy alone rather than textual-historical accuracy, which would not change its legal force but would change its scholarly defensibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accretion_vs_essence_historicity, empirical, 'Whether caste-hierarchical textual content is later accretion or original scriptural content.').

omega_variable(
    constitutional_supremacy_vs_textual_autonomy,
    'Should constitutional equality principles function as an external constraint on permissible textual interpretation, or does treating scripture as subordinate to constitutional norms itself constitute a category error about the nature of religious authority?',
    'No empirical resolution exists; this is a foundational jurisprudential and theological question about the relationship between state constitutional authority and religious textual authority.',
    'Adopting constitutional supremacy as the governing frame validates court intervention in doctrinal disputes; rejecting it would return interpretive authority to internal religious bodies regardless of equality outcomes, sharply reducing this reading''s institutional force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_supremacy_vs_textual_autonomy, conceptual, 'Whether constitutional equality can legitimately override internal religious-textual interpretive authority.').

omega_variable(
    reformist_reading_as_alternative_hegemony,
    'Does the reformist reading, once institutionally entrenched in courts and reform bodies, itself become a new interpretive monopoly that forecloses other egalitarian-compatible readings (such as the bhakti reading) from competing on their own terms?',
    'Track whether court decisions grounded in the reformist reading cite or accommodate devotional/bhakti-based egalitarian arguments, or whether they exclusively use constitutional-equality reasoning.',
    'If the reformist reading crowds out devotional egalitarian arguments in legal forums, its coexistence with the bhakti reading is more nominal than structural — despite both being egalitarian in effect, only one commands enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_as_alternative_hegemony, conceptual, 'Whether reformist legal dominance marginalizes the devotional alternative despite shared egalitarian ends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(vedi_tr_t35, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(vedi_tr_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(vedi_be_t35, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 35, 0.39).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(vedi_be_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(vedi_su_t35, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 35, 0.43).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(vedi_su_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 70, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vedic_dharmic_corpus kernel. hereditary_monopoly_reading and bhakti_devotional_reading are separate constraint files with their own epsilon values and stakeholder structures. This reading is distinguished by moderate extractiveness (~0.45), an inverted beneficiary structure relative to the hereditary reading (Dalit movements and reform courts benefit; orthodox institutions pay), and entanglement with state constitutional enforcement machinery not present in the devotional reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
