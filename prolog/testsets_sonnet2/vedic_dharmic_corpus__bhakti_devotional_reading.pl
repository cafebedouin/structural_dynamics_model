% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Reading of the Vedic-Dharmic Corpus
 *   domain: religious_authority/social_stratification
 *
 * SUMMARY:
 *   This story instantiates the bhakti devotional reading of the contested
 *   Vedic-dharmic kernel: the claim that sincere devotion, not birth,
 *   determines spiritual authority, and that direct devotional access to the
 *   divine bypasses caste requirements for ritual legitimacy. This is one
 *   reading among several sharing the same underlying textual and traditional
 *   kernel (the corpus of Vedic and dharmic teaching on ritual authority and
 *   caste). The hereditary monopoly reading holds the opposite premise — that
 *   varna hierarchy is divinely ordained and textually prescribed, with
 *   ritual authority strictly derived from Brahmin birth. The reformist
 *   egalitarian reading holds a third premise — that caste hierarchy is a
 *   historical accretion to be corrected by constitutional and rational
 *   critique rather than resolved through devotional theology at all. Each
 *   reading is authored as its own constraint with its own epsilon; this file
 *   addresses only the bhakti reading's structure as the bhakti tradition
 *   itself understands and practices it.
 *
 * KEY AGENTS:
 *   - non_brahmin_saint_poets: agenda_setter (moderate/mobile) — set the interpretive and practical terms of devotional access
 *   - bhakti_movement_devotees: primary beneficiary (powerless/mobile) — gain a legitimacy path previously denied
 *   - lower_caste_ritual_specialists_excluded_from_priesthood: primary payer (powerless/constrained) — devotion does not convert to institutional office
 *   - women_denied_formal_initiation_in_some_bhakti_lineages: secondary payer (powerless/trapped) — gender gate persists within the devotional frame
 *   - brahmin_ritual_establishment: excluded party (institutional/arbitrage) — sidestepped rather than confronted
 *   - religious_studies_scholars: analytical observer (analytical/analytical) — assesses actual mobility versus legitimating discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Reading of the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious_authority/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '833a7c33-f464-47b3-8b35-edb679b8b0e4').
narrative_ontology:cs_kernel_codification('833a7c33-f464-47b3-8b35-edb679b8b0e4', distributed).
narrative_ontology:cs_authority_grounding('833a7c33-f464-47b3-8b35-edb679b8b0e4', practice).
narrative_ontology:cs_interpretation_layer_present('833a7c33-f464-47b3-8b35-edb679b8b0e4').
narrative_ontology:cs_reading_relation('833a7c33-f464-47b3-8b35-edb679b8b0e4', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('833a7c33-f464-47b3-8b35-edb679b8b0e4', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('833a7c33-f464-47b3-8b35-edb679b8b0e4', foundational, devotional_sincerity_confers_spiritual_authority).
narrative_ontology:cs_axiom_status(devotional_sincerity_confers_spiritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('833a7c33-f464-47b3-8b35-edb679b8b0e4', devotional_sincerity_confers_spiritual_authority, deontological).
narrative_ontology:cs_axiom('833a7c33-f464-47b3-8b35-edb679b8b0e4', secondary, birth_status_is_not_dispositive_of_ritual_legitimacy).
narrative_ontology:cs_axiom_status(birth_status_is_not_dispositive_of_ritual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('833a7c33-f464-47b3-8b35-edb679b8b0e4', birth_status_is_not_dispositive_of_ritual_legitimacy, conventional).
narrative_ontology:cs_reference_frame('833a7c33-f464-47b3-8b35-edb679b8b0e4', pre_bhakti_ritual_exclusivity).
narrative_ontology:cs_drift_state('833a7c33-f464-47b3-8b35-edb679b8b0e4', contemporary_devotional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('833a7c33-f464-47b3-8b35-edb679b8b0e4', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_movement_devotees).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_saint_poets).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, temple_congregations_of_mixed_caste).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_ritual_specialists_excluded_from_priesthood).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, women_denied_formal_initiation_in_some_bhakti_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, temple_congregations_of_mixed_caste).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice direct devotional worship (kirtan, japa, personal surrender to a chosen deity) without requiring a Brahmin intermediary, Sanskrit literacy, or caste-verified lineage. Their spiritual standing is asserted through the intensity and sincerity of devotion rather than birth status, giving them a path to religious authority previously closed to them.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_movement_devotees, beneficiary,
    powerless, biographical, mobile, regional).

% Figures such as vernacular poet-saints who composed and taught devotional theology in local languages, establishing that spiritual authority could be earned through devotion and teaching rather than inherited. They set the interpretive terms of this reading by producing the canon of songs, commentary, and practice that devotees follow, and their historical example is cited as living proof that the caste requirement is not absolute.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, non_brahmin_saint_poets, agenda_setter,
    moderate, civilizational, mobile, regional).

% Worship together across caste lines within bhakti temple communities that admit devotees on the basis of devotion rather than birth. They gain access to communal worship and status previously denied, but many still find that senior ritual or administrative roles within these same temples quietly revert to caste-privileged appointees, so the promised equality is only partially realized in institutional practice.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, temple_congregations_of_mixed_caste, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, temple_congregations_of_mixed_caste, payer).

% Even within bhakti communities that formally reject caste as a spiritual barrier, the formal priesthood, temple administration, and Sanskrit textual authority frequently remain staffed by Brahmin lineage-holders. Devotional sincerity opens personal worship but does not reliably convert into institutional office; they bear the residual caste hierarchy that the devotional reading does not fully dissolve.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_ritual_specialists_excluded_from_priesthood, payer,
    powerless, biographical, constrained, local).

% Though bhakti theology is claimed to be open to devotion regardless of birth status, gender still operates as a secondary gate in many lineages: formal guru initiation, textual teaching authority, or leadership of congregations remains restricted. They bear a cost the devotional reading's own universalist claim does not account for.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, women_denied_formal_initiation_in_some_bhakti_lineages, payer,
    powerless, biographical, trapped, local).

% Holds the hereditary claim to ritual and textual authority that the bhakti reading structurally sidesteps rather than confronts. This reading does not require their overthrow — it simply asserts an alternate, parallel path to legitimacy — so the establishment is not directly addressed as a party to be persuaded or defeated within this constraint, even though their institutional position is what devotional access is implicitly bypassing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_ritual_establishment, excluded,
    institutional, generational, arbitrage, regional).

% Study the historical bhakti movements as social and theological phenomena, assessing how much actual caste mobility resulted from devotional theology versus how much operated as a spiritually legitimating discourse layered atop persistent social hierarchy. They can compare bhakti communities' stated egalitarianism against their internal leadership and marriage patterns over time.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuine alternative path to religious legitimacy and community belonging for people excluded from Brahmin-mediated ritual access — devotion, vernacular teaching, and personal relationship with the divine substitute for lineage-verified ritual competence, solving the real problem of spiritual exclusion without requiring wholesale reform of caste-based ritual institutions.
% TRANSFER_FUNCTION: Moves religious authority and community standing away from birth-verified lineage and toward demonstrated devotional practice; moves some material support (temple offerings, patronage, teaching income) toward non-Brahmin poet-saints and devotional leaders who would otherwise have no claim to it.
% ABSENT_VOICES: The Brahmin ritual establishment is not directly confronted by this reading — it is sidestepped rather than argued with, so their objection (that legitimate ritual authority requires proper lineage and training) is not adjudicated here. Women in restrictive bhakti lineages, and lower-caste specialists who want institutional office rather than personal devotion, are also underrepresented in the movement's own self-description as fully egalitarian.
% DISAPPEARANCE_RATIONALE: If the bhakti devotional legitimation path vanished, the many communities, temple lineages, vernacular literary traditions, and forms of popular religious practice built on it would lose their claim to legitimate spiritual standing; congregants without Brahmin lineage would have no alternative route to religious authority within the tradition, and centuries of devotional literature and institutional practice would need new grounding or would collapse back toward hereditary-only legitimacy.
% FOUNDING_PROBLEM: Large populations were structurally excluded from ritual participation and spiritual authority by birth-based caste rules; bhakti movements arose to provide direct, unmediated access to the divine that did not depend on caste status, Sanskrit literacy, or priestly intermediation.
% FOUNDING_PROBLEM_CORROBORATION: Devotional communities and their historians attest the problem remains partially live wherever caste-based exclusion from ritual office persists; independent religious studies scholarship (outside both the bhakti movements and the Brahmin establishment) corroborates that historical bhakti did expand practical access to religious participation while noting institutional leadership often reverted to caste-privileged actors — so the founding problem was substantially, but not fully, resolved by the movement's own success.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.40 at interval end) because the bhakti reading is substantially a genuine coordination mechanism — it opens real spiritual and communal participation to excluded groups — but it does not fully dissolve caste hierarchy; some residual extraction persists where institutional office, priesthood, and formal initiation still track caste and gender lines despite the theology's universalist claim. Suppression is comparatively low (0.35) because bhakti communities generally do not coerce participation or actively suppress alternative paths; the mechanism operates through persuasion, devotional practice, and vernacular teaching rather than enforcement. Theater ratio is moderate-low but rising slightly over the interval (0.15 to 0.30) as some institutionalized bhakti lineages develop their own hereditary guru successions and temple hierarchies that perform egalitarian rhetoric while reproducing caste-adjacent leadership patterns. Accessibility collapse is moderate (0.40): alternatives (hereditary ritual paths, reformist critique) remain visible and available, they are not suppressed by this reading, consistent with a rope rather than snare or tangled_rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From the devotee and saint-poet seats, the arrangement reads as liberating coordination — a real alternative path to spiritual legitimacy that did not exist before. From the seat of a lower-caste aspirant to priesthood or a woman seeking initiation, the same theological claim can read as a partial promise: devotion is declared sufficient, but institutional practice still asks for more. The engine's seat-level computation should surface this divergence directly from the declared power/exit differences rather than from any claim asserted in the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Brahmin saint-poets and ordinary devotees are the structural beneficiaries — the reading gives them a legitimacy claim they did not previously hold, so their derived directionality sits toward the beneficiary end. Lower-caste ritual specialists seeking institutional office and women seeking formal initiation are payers within this same reading: they experience the promise of caste-blind devotion without the corresponding institutional access, so their directionality sits closer to the target end despite the reading's egalitarian self-description. The Brahmin establishment is deliberately not classified as payer or beneficiary here — the bhakti reading does not extract from them or transfer their position; it constructs a parallel track. This is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusion from ritual participation by birth — is only partially resolved: bhakti communities materially expanded participation and spiritual standing for excluded groups, satisfying much of the original coordination need, but the persistence of caste-and-gender-inflected institutional leadership within ostensibly egalitarian bhakti lineages shows the mandate is not fully discharged. Classifying this as rope (not scaffold) is deliberate: the reading does not carry a declared sunset condition and its proponents do not treat it as transitional — it is presented as a standing, permanent theological truth, even though its practical achievement remains incomplete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    devotional_access_vs_institutional_office,
    'Does bhakti''s claim that devotion determines spiritual authority actually convert into institutional access (priesthood, temple administration, formal teaching authority), or does it remain confined to personal/informal spiritual standing while formal office continues to track caste lineage?',
    'Historical and sociological survey of bhakti temple leadership, guru lineages, and institutional appointments over multiple centuries and regions, compared against caste composition of congregations.',
    'If institutional office remained caste-locked despite devotional theology, the reading''s practical extraction is higher than its theological self-description suggests, sharpening the case for a tangled_rope reading in specific institutionalized bhakti lineages even while the informal devotional practice remains rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_access_vs_institutional_office, empirical, 'Whether devotional legitimacy converts into actual institutional access or stays informal.').

omega_variable(
    gender_gate_within_devotional_universalism,
    'Is the exclusion of women from formal initiation in some bhakti lineages a contingent historical accretion within the movement, or a structural feature of how devotional authority is transmitted (guru-disciple lineage requiring male succession)?',
    'Comparative study of bhakti lineages that do versus do not restrict female initiation, examining whether restriction correlates with specific textual or institutional features versus regional custom.',
    'If structural, the devotional reading''s claim to bypass ascriptive hierarchy entirely is weaker than presented, and the victim set for gender should be treated as a stable rather than declining feature of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_gate_within_devotional_universalism, conceptual, 'Whether gender exclusion is contingent custom or structural to devotional transmission.').

omega_variable(
    sincerity_as_unverifiable_criterion,
    'Since ''sincere devotion'' cannot be externally verified the way lineage can, does this create a different kind of gatekeeping — where community leaders or established devotees informally judge sincerity, potentially reproducing social biases (including caste bias) under a different vocabulary?',
    'Ethnographic study of how bhakti communities actually adjudicate claims to devotional sincerity and spiritual advancement, and whether such judgments correlate with caste, gender, or class background of the claimant.',
    'If sincerity-judgment reproduces the same social sorting as lineage did, the reading''s coordination function is partly cosmetic, pushing this constraint''s computed type toward tangled_rope; if judgments are genuinely decoupled from prior social status, the rope classification is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_as_unverifiable_criterion, empirical, 'Whether unverifiable sincerity criteria quietly reintroduce social gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vedic_dharmic_corpus kernel. hereditary_monopoly_reading holds ritual authority is fixed by Brahmin birth (epsilon high, snare/tangled_rope-leaning, clear beneficiary class in the hereditary priesthood). reformist_egalitarian_reading holds caste hierarchy is historical accretion correctable by constitutional/rational critique, independent of devotional theology (epsilon low on its own terms, oriented toward abolishing rather than parallel-tracking the hierarchy). This bhakti reading sits structurally between them: moderate epsilon (~0.40), no concentrated beneficiary class capturing rents, and a victim set that shrinks relative to the hereditary reading but does not disappear, because institutional office and gender-based initiation gates persist within many devotional lineages despite the theology's universalist claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
