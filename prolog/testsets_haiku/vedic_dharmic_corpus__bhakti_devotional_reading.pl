% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine Authority
 *   domain: religious/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   The bhakti devotional reading of the Vedic-Dharmic corpus claims that
 *   sincere devotion (bhakti) to the divine — demonstrated through emotional
 *   authenticity, practice discipline, and lived devotion — provides direct
 *   access to divine grace and spiritual authority independent of caste
 *   birth. This reading challenges the hereditary monopoly reading that ties
 *   ritual authority and spiritual legitimacy to brahminical birth while
 *   remaining within the same inherited textual tradition. The bhakti reading
 *   operates as a coordination solution within an authority structure (the
 *   Vedic corpus) but redistributes legitimacy away from hereditary
 *   gatekeeping toward devotional practice accessible to all castes. The
 *   constraint is CLAIMED as rope (genuine coordination around sincere
 *   devotion as a legitimacy criterion) while authored metrics show moderate
 *   extractiveness declining over time — the engine will measure whether the
 *   coordination framing accurately captures the structure or masks continued
 *   exclusion.
 *
 * KEY AGENTS:
 *   - sincere_devotees_all_castes: Practitioners across caste boundaries who gain spiritual legitimacy and divine access through devotional sincerity rather than birth.
 *   - hereditary_brahmin_priests: Organized clergy whose exclusive gatekeeping authority is challenged; they maintain ritual role but lose monopoly claim.
 *   - temple_institutions: Agenda-setters transmitting the bhakti reading; sites of coexistence and negotiation between hereditary and devotional framings.
 *   - non_brahmin_merchants_farmers: Powerful economic actors who gain spiritual authority and public legitimacy through devotional movements.
 *   - lower_caste_practitioners: Constrained but mobile beneficiaries of spiritual access; material hierarchy persists despite spiritual inclusion.
 *   - vedic_textual_authority: The inherited corpus itself — the kernel both readings claim to instantiate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/social_stratification/interpretive_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed').
narrative_ontology:cs_kernel_codification('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', fixed_text).
narrative_ontology:cs_authority_grounding('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', lineage).
narrative_ontology:cs_interpretation_layer_present('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed').
narrative_ontology:cs_reading_relation('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', foundational, sincere_devotion_supersedes_birth).
narrative_ontology:cs_axiom_status(sincere_devotion_supersedes_birth, holdable).
narrative_ontology:cs_axiom_grounding('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', sincere_devotion_supersedes_birth, deontological).
narrative_ontology:cs_axiom('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', foundational, direct_divine_access_available_all_sincere).
narrative_ontology:cs_axiom_status(direct_divine_access_available_all_sincere, holdable).
narrative_ontology:cs_axiom_grounding('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', direct_divine_access_available_all_sincere, empirically_contingent).
narrative_ontology:cs_reference_frame('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', devotional_sincerity_as_authority_criterion).
narrative_ontology:cs_drift_state('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', contemporary_modern_hindu_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0f5cdbd5-5f7a-4f99-9974-7b6dda77e9ed', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_all_castes).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_merchants_farmers).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_practitioners).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priests).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, direct_devotional_accessibility).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotion_supersedes_birth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access direct relationship with divine through sincere devotional practice (bhakti) without requiring brahminical ritual mediation or caste status. Sincere devotion is the measure of spiritual authority and access, not birth. They gain a pathway to spiritual legitimacy that does not depend on institutional gatekeeping by hereditary priesthood.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_all_castes, beneficiary,
    moderate, biographical, mobile, regional).

% Their exclusive interpretive and ritual monopoly is challenged by devotional movements that claim non-brahmin practitioners can reach divine directly. They do not lose their role entirely — bhakti reading does not foreclose brahminical ritual — but their gatekeeping authority over spiritual legitimacy is reduced. Maintaining authority requires them to acknowledge sincere devotion as valid, which shrinks their monopoly rent.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_priests, payer,
    organized, generational, constrained, regional).

% Operate as transmission sites for the bhakti reading. They frame devotional access as open to sincere practitioners of any caste, drawing lower-caste and merchant-class devotees. They also employ and depend on brahmin priests, creating internal tension: acknowledging devotional legitimacy opens access but requires negotiating with the hereditary priesthood who staff ritual functions. Temples become sites where the two readings coexist under strain.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, temple_institutions, agenda_setter,
    institutional, generational, constrained, regional).

% Wealthy merchant and landowner classes gain spiritual legitimacy and public authority through devotional movements without needing brahminical validation. The bhakti reading allows them to sponsor temples, commission devotional texts, and claim spiritual authority grounded in sincere devotion rather than caste. Their economic power finds a legitimacy path the hereditary system denied.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_merchants_farmers, beneficiary,
    powerful, biographical, mobile, regional).

% Gain access to devotional spirituality and spiritual dignity through bhakti movements that claim sincere devotion transcends caste. However, the constraint operates at the level of spiritual authority and ritual access; material caste hierarchy (occupational restrictions, social segregation, economic exclusion) persists. Spiritual access does not eliminate social subordination.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% The Vedas, Upanishads, and Bhagavad Gita themselves — the inherited textual corpus that both readings claim to instantiate. Both the hereditary and bhakti readings cite the same texts but interpret them differently. The texts do not speak univocally; interpretation mediates between the text and contemporary authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_textual_authority, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_textual_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of spiritual legitimacy in a society with inherited scriptural authority but diverse lived experience: bhakti reading offers a coordination solution where devotional sincerity (observable through conduct, emotional authenticity, and practice) becomes the measure of spiritual authority, rather than birth status. This allows spiritual authority to exist outside hereditary gatekeeping while still respecting the inherited textual tradition.
% TRANSFER_FUNCTION: Transfers spiritual authority and public legitimacy from hereditary priesthood to devotional practitioners of any caste who demonstrate sincere devotion. Non-brahmin wealthy patrons and lower-caste devotees gain access to ritual sponsorship, teaching authority, and spiritual dignity. The priesthood does not lose its role but its monopoly shrinks: they become custodians of ritual technique rather than sole gatekeepers of divine access.
% ABSENT_VOICES: Hereditary brahmins whose livelihoods depend on monopoly gatekeeping are structurally overrepresented in textual authority (they are the primary readers and interpreters of the corpus). Lower-caste practitioners historically had no voice in textual interpretation until devotional movements gave them one. Contemporary rationalist and secular critiques are absent — they would reject both this reading and the hereditary reading as equally mythological.
% DISAPPEARANCE_RATIONALE: If the bhakti reading vanished and the hereditary monopoly returned unchallenged, temple authority would revert entirely to brahmins; non-brahmin spiritual teachers would lose public legitimacy; merchant and farmer classes would lose the pathway to spiritual authority and public honor devotional movements provided. The social structure would not collapse, but authority distribution and institutional alignment would shift.
% FOUNDING_PROBLEM: How can a society maintain inherited scriptural authority while accommodating observed spiritual authenticity that does not match hereditary criteria? The bhakti reading answers: sincere devotion to the divine, demonstrated through practice and emotional authenticity, supersedes birth status as the criterion for spiritual legitimacy and direct divine access.
% FOUNDING_PROBLEM_CORROBORATION: Devotional movements in South India (Alvar and Nayanar poets, 6th–10th centuries onward) and North India (Bhakti saints, 15th–18th centuries) attest that sincere devotion produces recognized spiritual authority outside brahminical credentials. Medieval temple records show non-brahmin devotees exercising teaching and ritual roles. Contemporary Hindu practitioners across caste lines cite this reading as normative. Scholars of bhakti traditions (outside the hereditary priesthood) document the reading as a historical fact of religious practice, though they dispute its scriptural foundation.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.40) and declining across the interval because the bhakti reading does reduce the hereditary priesthood's extraction from spiritual legitimacy — it opens access and shifts authority criterion from unchallengeable birth to observable devotion. However, the constraint persists because even sincere devotion must be adjudicated by someone (temples, established teachers), and temples depend on brahmin priests for ritual functions; the constraint does not eliminate institutional gatekeeping, it changes the gate's logic. Theater is low-to-moderate (0.25) because the devotional function is genuine and widely practiced, not theatrical, but a secondary function is legitimizing continued institutional dependence even as the access criterion shifts. Suppression is moderate (0.35) and declining because the bhakti reading gains social force through genuine appeal (not coerced), and active resistance from hereditary priesthood is constrained by the reading's scriptural plausibility and popular legitimacy. The measurement series shows extractiveness and suppression declining toward a floor (~0.40 / ~0.35) as the reading becomes normalized — the constraint stabilizes at a lower extraction level than the hereditary monopoly would maintain. All metrics share the same time grid (six points at 0, 10, 20, 30, 40, 50), documented as observed across the historical interval.
 *
 * PERSPECTIVAL GAP:
 *   A brahmin priest experiences this reading as erosion of their authority monopoly and constraint on their spiritual role (they must acknowledge non-brahmin devotees as spiritually legitimate). A sincere devotee of any caste experiences the reading as liberation from gatekeeping and validation of their direct divine relationship. A temple institution experiences it as both opportunity (expanded devotee base) and management problem (negotiating between hereditary and devotional authority claims). A secular rationalist observes both readings as mythological and both beneficiary claims as ideological cover. The engine computes per-seat type from the structural data; the authored rope claim captures only the beneficiary-side framing. Where computed type diverges from the claimed rope, that divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The structure is unlike snare or tangled rope because there is no clear captured beneficiary class or centralized victim: sincere_devotees_all_castes and non_brahmin_merchants_farmers gain legitimate access and authority, so they sit near beneficiary directionality (d toward 0.0); hereditary_brahmin_priests lose monopoly gatekeeping but retain ritual role, so they sit near symmetric (d around 0.5); lower_caste_practitioners gain spiritual dignity but not material freedom, so they sit in a contested zone where spiritual benefit and material subordination coexist (d moderate). No directionality override is needed because the derivation from the beneficiary declarations (empty — no agent class is universally benefited or victimized by the reading alone) + the power/exit structure produces the right result: moderate d across all seats, reflecting the reading's coordinating function rather than extractive structure. The key directionality fact is the SHIFT in who can hold spiritual authority: from hereditary monopoly to devotional sincerity. This is a genuine reallocation of authority, not a zero-sum extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would apply if the founding problem (how to maintain scriptural authority while accommodating observed spiritual authenticity outside hereditary criteria) became structurally irrelevant — that is, if scriptural authority itself lost social force or if the hereditary criterion became so weakened that devotional sincerity no longer needed to claim scriptural legitimacy. Contemporary evidence suggests the founding problem remains live: Hindu communities across South and North India continue to contest the bhakti reading as against hereditary authority, and both readings cite the same texts as foundational. The reading has not atrophied into theater; it remains functionally engaged as a legitimacy claim. However, a secondary mandatrophy signal exists: in secular-majority contexts (urban India, diaspora), the devotional reading increasingly operates more as inherited identity marker than as live contested authority — it becomes 'this is our tradition' rather than 'devotion trumps birth.' If that shift completed, the constraint would persist as cultural identity rather than live authority negotiation, which would be a form of theater-driven piton. The measurement series stabilization toward 0.40/0.35 suggests the reading is approaching a steady state where it has reshaped the authority landscape without displacing the scriptural kernel entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    devotional_sincerity_measurement,
    'What constitutes ''sincere devotion'' sufficiently authentic to count as spiritual authority? Who adjudicates sincerity?',
    'Examination of how temple institutions, established teachers, and devotional communities actually distinguish sincere from insincere or performative devotion; what conduct or practice is accepted as evidence.',
    'If sincerity is subjective and adjudicated by the same gatekeepers as the hereditary system, the constraint becomes a rebranded monopoly (snare). If sincerity has objective markers (emotional authenticity, practice discipline, public recognition), the rope framing holds. If different communities disagree on sincerity criteria, the constraint fragments into competing local constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_sincerity_measurement, conceptual, 'The measurement problem: how sincere devotion is recognized and who decides.').

omega_variable(
    spiritual_vs_material_liberation,
    'Does the bhakti reading''s claim to spiritual authority independent of caste birth constitute liberation from caste hierarchy, or does it leave material caste hierarchy intact?',
    'Historical and contemporary evidence of whether non-brahmin devotees who gain spiritual authority also gain material resources, occupational mobility, or social standing equivalent to brahmin equivalents.',
    'If spiritual authority translates to material benefit and occupational mobility, the reading is genuinely transformative. If spiritual dignity coexists with material subordination (the devotee is respected spiritually but remains ritually impure, occupationally constrained, and socially excluded in non-devotional contexts), the constraint is extraction disguised as liberation — the extractiveness is higher than 0.40 and the constraint may reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spiritual_vs_material_liberation, empirical, 'Whether bhakti authority gains translate to material caste-hierarchy transformation.').

omega_variable(
    scriptural_foundation_contested,
    'Is sincere devotion superseding birth as the criterion for spiritual authority actually grounded in the Vedic-Dharmic texts, or is the bhakti reading a reinterpretation that reads modern egalitarian values back into an ancient hierarchical text?',
    'Textual scholarship and cross-tradition comparison: what do the Upanishads, Bhagavad Gita, and Puranas actually say about devotion, caste, and spiritual authority? Do the texts support non-brahmin spiritual authority or does brahminical monopoly have stronger textual warrant?',
    'If the texts substantially support devotional access independent of birth, the hereditary reading is the reinterpretation and the bhakti reading is a legitimate reading of the kernel. If the texts are ambiguous or do not clearly support non-brahmin authority, both readings are reinterpretations and the kernel is the site of genuine interpretive struggle. This is the foundational uncertainty in the constraint''s claim to authenticity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_foundation_contested, conceptual, 'Whether the bhakti reading''s scriptural warrant is genuine or reinterpretive.').

omega_variable(
    kernel_reading_relationship,
    'As a reading of the vedic_dharmic_corpus kernel, does the bhakti_devotional_reading coexist with the hereditary_monopoly_reading or does one reading''s core claim logically foreclose the other?',
    'Examination of whether a single framework (a unified interpretation of the Vedic corpus) can hold both ''sincere devotion supersedes birth'' AND ''varna hierarchy is divinely ordained,'' or whether commitment to one requires rejection of the other.',
    'If they foreclose each other, only one can be instantiated in a single authority structure. If they coexist, both readings persist simultaneously in different institutional seats or in contested negotiation within temples. The constraint''s type may depend on whether the reading is in coherent coexistence with hereditary authority (rope) or in fundamental conflict with it (tangled_rope with active enforcement against hereditary claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'The logical relationship between bhakti and hereditary readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(vedi_tr_t10, observed).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(vedi_tr_t20, observed).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t30, observed).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t40, observed).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(vedi_be_t10, observed).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(vedi_be_t20, observed).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(vedi_be_t30, observed).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement_basis(vedi_be_t40, observed).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement_basis(vedi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(vedi_su_t10, observed).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(vedi_su_t20, observed).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement_basis(vedi_su_t30, observed).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement_basis(vedi_su_t40, observed).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement_basis(vedi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel decomposes into three constraint stories — three readings of the same inherited authority structure, each with different epsilon values and beneficiary/victim mappings. The bhakti_devotional_reading (this story) claims moderate extractiveness (ε ~0.40) and genuine coordination function; the hereditary_monopoly_reading claims lower extractiveness from the hereditary perspective (ε ~0.25, natural law framing) but higher extractiveness from excluded lower-caste perspective; the reformist_egalitarian_reading claims the entire inherited structure is extractive regardless of framing (ε ~0.65+, snare). Each reading instantiates a different constraint with different stakes. The three are linked by network.affects_constraints: bhakti influences both sibling readings by offering a middle path that neither hereditary nor reformist fully accepts, and by shifting the legitimacy criterion away from birth toward devotion, creating structural pressure on hereditary gatekeeping (influences hereditary_monopoly_reading) and legitimacy competition with secular egalitarianism (influences reformist_egalitarian_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
