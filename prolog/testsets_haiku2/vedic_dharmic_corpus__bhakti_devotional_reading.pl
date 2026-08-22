% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Bhakti Devotional Access to Divine Authority
 *   domain: religious/authority/social_stratification
 *
 * SUMMARY:
 *   The bhakti devotional reading of Vedic-Dharmic scripture asserts that
 *   sincere devotion (bhakti) to the divine provides direct spiritual access
 *   that bypasses caste-birth requirements and Brahmin ritual mediation. This
 *   is ONE reading of a contested kernel — the Vedic-Dharmic corpus itself —
 *   which is also read as legitimating hereditary caste hierarchy and as
 *   demanding constitutional equality. The bhakti reading emerged
 *   historically in medieval South India and has remained a live theological
 *   position alongside hereditary and reformist readings for roughly 1500
 *   years. This story models the constraint as a COORDINATION mechanism
 *   (rope): bhakti solves the problem of how sincere seekers can access
 *   divinity without institutional priesthood, and it distributes spiritual
 *   authority to devotees across castes rather than concentrating it in
 *   Brahmin lineage. However, it is NOT pure coordination — lower-caste
 *   devotees gain authority in the devotional sphere but remain materially
 *   and legally constrained in the non-devotional caste hierarchy, and
 *   Brahmin ritual specialists bear the cost of lost monopoly authority. The
 *   claim (rope) and the metrics (moderate extractiveness 0.40, low
 *   suppression 0.35) reflect that the arrangement is genuinely coordinating
 *   while asymmetrically benefiting some seats at the cost of others'
 *   authority claims.
 *
 * KEY AGENTS:
 *   - sincere_devotees_across_castes: beneficiaries of direct divine access outside hereditary mediation (powerless/mobile); the reading's primary constituency
 *   - hereditary_brahmin_ritual_specialists: lose exclusive control over spiritual authority and ritual mediation; identity-locked to Brahmin status (organized/identity-locked)
 *   - lower_caste_aspirants: gain spiritual authority and community teaching roles through devotion, though not material freedom from caste occupation/restriction (moderate/constrained)
 *   - brahmin_reform_interpreters: Brahmin scholars who adopt bhakti reading and gain moral prestige by reconciling tradition with egalitarian practice (powerful/mobile)
 *   - non_devotional_caste_hierarchy_enforcers: excluded from theological conversation; their material caste enforcement is not directly challenged by devotional egalitarianism (institutional/trapped)
 *   - colonial_and_modern_reform_authorities: observe and provide legal recognition framework that enables bhakti movements (institutional/analytical)
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
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious/authority/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '6bde1987-9514-4d09-896a-b4c8522ff811').
narrative_ontology:cs_kernel_codification('6bde1987-9514-4d09-896a-b4c8522ff811', fixed_text).
narrative_ontology:cs_authority_grounding('6bde1987-9514-4d09-896a-b4c8522ff811', lineage).
narrative_ontology:cs_interpretation_layer_present('6bde1987-9514-4d09-896a-b4c8522ff811').
narrative_ontology:cs_reading_relation('6bde1987-9514-4d09-896a-b4c8522ff811', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bde1987-9514-4d09-896a-b4c8522ff811', vedic_dharmic_corpus__reformist_egalitarian_reading, influences).
narrative_ontology:cs_axiom('6bde1987-9514-4d09-896a-b4c8522ff811', foundational, sincere_devotion_suffices_for_authority).
narrative_ontology:cs_axiom_status(sincere_devotion_suffices_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('6bde1987-9514-4d09-896a-b4c8522ff811', sincere_devotion_suffices_for_authority, deontological).
narrative_ontology:cs_axiom('6bde1987-9514-4d09-896a-b4c8522ff811', foundational, direct_divine_access_independent_of_birth).
narrative_ontology:cs_axiom_status(direct_divine_access_independent_of_birth, holdable).
narrative_ontology:cs_axiom_grounding('6bde1987-9514-4d09-896a-b4c8522ff811', direct_divine_access_independent_of_birth, deontological).
narrative_ontology:cs_reference_frame('6bde1987-9514-4d09-896a-b4c8522ff811', vedic_devotional_tradition_egalitarianism).
narrative_ontology:cs_drift_state('6bde1987-9514-4d09-896a-b4c8522ff811', contemporary_post_colonial_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6bde1987-9514-4d09-896a-b4c8522ff811', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_across_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_reform_interpreters).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_ritual_specialists).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct access to divine experience through sincere devotion (bhakti), bypassing ritual-mediation requirements and caste birth restrictions. Their spiritual authority derives from the authenticity and intensity of their devotional practice, not from lineage. They may form their own spiritual communities and teach others who share their devotional orientation, creating alternative authority structures that do not require Brahmin gatekeeping.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_across_castes, beneficiary,
    powerless, biographical, mobile, regional).

% Bear the cost of losing exclusive control over direct divine access and ritual mediation. Their structural authority derives from birth into Brahmin lineage and mastery of Vedic ritual; the bhakti reading asserts that sincere devotion can accomplish what Brahmin mediation claims to enable. Their monopoly on interpretive authority is contested, though their institutional prestige and property holdings remain largely protected. Exit would require renouncing their entire identity and social position within the hereditary order.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_brahmin_ritual_specialists, payer,
    organized, generational, identity_locked, regional).

% Gain access to spiritual authority and community leadership through devotional practice, which the hereditary reading denies them. They also bear costs: adopting the bhakti reading requires accepting a subordinate social position in the wider non-devotional society, and devotional communities themselves may reproduce gender hierarchies and internal status differentials even while rejecting caste birth requirements. Their exit options are constrained by land ownership patterns, occupational restrictions, and limited geographic mobility.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__bhakti_devotional_reading, lower_caste_aspirants, payer).

% Brahmin scholars and spiritual teachers who embrace the bhakti reading and advocate for direct devotional access. They gain moral authority and intellectual prestige by reconciling Vedic texts with egalitarian religious practice. They can teach across caste lines and attract devoted followers, expanding their influence beyond hereditary congregation. They are not locked into the hereditary monopoly position and can exit by adopting reform interpretations.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, brahmin_reform_interpreters, beneficiary,
    powerful, generational, mobile, regional).

% Village councils, landlords, and political authorities who enforce caste occupation restrictions and ritual purity rules in non-devotional contexts (land access, marriage, occupation, ritual purity). The bhakti reading's spiritual authority does not directly displace their enforcement, but it provides an alternative legitimacy framework that delegitimizes the caste system's foundational claims. They would object that devotional exemptions undermine the varna order that justifies their own authority, but they are excluded from the theological conversation that the bhakti reading occupies.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, non_devotional_caste_hierarchy_enforcers, excluded,
    institutional, generational, trapped, regional).

% British administrators, Hindu reform movements, and post-independence constitutional authorities who document, record, and sometimes mandate the recognition of bhakti movements and their spiritual legitimacy. They provide external validation and legal framework that enables the bhakti reading to persist and spread even as traditional institutions resist it. Their analytical position is neither devotional nor hereditary; they observe and adjudicate.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, colonial_and_modern_reform_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, sincere_devotees_across_castes).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bhakti devotional practice solves the coordination problem of spiritual legitimacy and direct divine access: instead of mediation through a professional class (Brahmin ritual specialists), sincere devotion becomes the coordinating mechanism. Devotees self-verify their spiritual progress through internal experience and communal recognition; no institutional priesthood is required to certify their connection to the divine.
% TRANSFER_FUNCTION: Moves spiritual authority and social prestige from hereditary Brahmin lineage to demonstrated devotional sincerity. Also redistributes teaching authority: bhakti-path teachers gain followers across caste lines, creating alternative institutional structures that compete with hereditary ritual specialists. The arrangement transfers social mobility opportunities to lower-caste aspirants in the devotional sphere, though not in the wider society.
% ABSENT_VOICES: Non-devotional populations locked in caste hierarchy (landless laborers, occupational castes denied mobility, women in restrictive marriage regimes) are structurally excluded from the theological conversation. Their voices would object that spiritual egalitarianism in the devotional sphere does not address material inequality and occupation-based discrimination in the non-devotional economy. They would question whether sincere devotion is truly accessible to those exhausted by caste labor or restricted by gender, and would demand material reform alongside spiritual reimagining.
% DISAPPEARANCE_RATIONALE: If bhakti devotional authority vanished as a competing legitimacy framework, the hereditary Brahmin monopoly on direct divine access and interpretive authority would consolidate. Devotional communities and their alternative authority structures (guru-lineages, assembly teachings, vernacular scripture interpretation) would lose their theological justification and would either dissolve into the hereditary hierarchy or persist as purely social/emotional practices without claims to spiritual authority. The reading's disappearance would leave the caste system without a major internal challenge to its legitimacy.
% FOUNDING_PROBLEM: Early medieval South Indian temple devotionalism and bhakti movements emerged to address the problem that ritual-mediation-based spiritual access was inaccessible to those excluded by birth from Brahmin status. The founding problem: How can sincere seekers of any caste access the divine directly, without requiring hereditary priestly mediation?
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movement historians and contemporary devotional practitioners attest the problem is live and ongoing: caste restrictions on temple access, ritual performance, and spiritual teaching persist in many contexts. Hereditary Brahmin authorities contest the problem framing, asserting that varna-based ritual specialization is divinely ordained and that devotional feeling is not a substitute for textual knowledge and ritual mastery. Constitutional-era reform authorities and independent religious scholars attest that the founding problem was materially true in pre-modern and colonial contexts, though its persistence in post-independence law is contested.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.40 reflects that bhakti offers genuine spiritual coordination (sincerity-based access is real, not theatrical) but does not resolve the material caste hierarchy. Lower-caste devotees gain authority in a devotional sphere but remain occupationally restricted and socially subordinate in non-devotional contexts — their gain is real but limited in scope. Brahmin ritual specialists lose authority-monopoly claims but retain property, social prestige, and can themselves become bhakti teachers if they adopt the reading (their cost is partly absorded by opportunity to transition). Suppression at 0.35 is moderate-low: the bhakti reading does not require coercive enforcement from devotees (sincere devotion is self-reinforcing), but its persistence against hereditary authority does depend on resistance to Brahmin objections and on social support (reform institutions, colonial record-keeping, constitutional frameworks). Theater at 0.25 is low-moderate: the devotional practice is genuine (people authentically experience spiritual intimacy), but devotional communities do perform their boundary (ritual dress, spiritual songs, guru reverence) in ways that somewhat substitute for the material redistribution they do not achieve. Accessibility collapse at 0.50: alternatives to devotion (Vedic ritual, philosophical inquiry, renunciation) remain live; devotion is available to sincere aspirants but requires cultural knowledge and time/resources to practice regularly, so the barrier is not near-complete but is real. Resistance at 0.60: the reading meets substantial resistance from hereditary authorities (who assert Brahmin expertise is irreplaceable) and from non-devotional caste hierarchy (which remains structurally intact); the reading persists despite this resistance because internal devotional experience is hard to suppress and because external reform authorities have validated it. The measurement series shows extractiveness holding stable around 0.40 (not rising dramatically, not disappearing) across the 30-year interval: the reading's accommodation within the broader Dharmic framework has stabilized, neither consolidating its authority dominance nor being displaced.
 *
 * PERSPECTIVAL GAP:
 *   The payer/beneficiary divide maps imperfectly to power and social position. Powerless devotees benefit most (spiritual egalitarianism is real at their scale). Organized Brahmin specialists pay most (authority loss is real and concentrated). But lower-caste aspirants simultaneously benefit (spiritual authority is new) and pay (they remain subordinate in non-devotional structures). The constraint does not resolve but rather DISPLACES the caste question: from 'are you born into authority?' to 'are you devotionally sincere?' This is real redistribution in the spiritual domain while the material hierarchy persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Sincere devotees are clear beneficiaries (direct divine access, spiritual authority, community membership): directionality for this group is low (near 0.0 — they experience subsidy/gain from the constraint). Brahmin ritual specialists are targets of the reading's redistribution (they lose monopoly, face authority challenges): their directionality is high (near 0.8–0.9), though not maximal because they retain social prestige and can themselves adopt the reading. Lower-caste aspirants are a MIXED group: they gain in the devotional sphere (beneficiary dynamics) but remain constrained in the material non-devotional hierarchy (target dynamics). Their directionality should split: ~0.4–0.5 depending on how much time they actually spend in devotional vs non-devotional roles. Brahmin reformers benefit from the reading (prestige, moral authority, broader teaching base) without losing Brahmin privilege: directionality should be low (~0.2–0.3), reflecting net gain. The measured extractiveness of 0.40 is an average across these heterogeneous seats, reflecting the rope-type asymmetry: genuine coordination that re-distributes authority, not pure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The bhakti reading's founding problem — how can sincere seekers access divinity without hereditary priesthood? — remains LIVE across the interval (contemporary devotional movements still exist and still make this claim, and hereditary authorities still resist). However, the reading's FUNCTION has partly shifted over time. In medieval South India, bhakti was a genuinely alternative institution that enabled lower-caste spiritual leadership and community formation. In post-colonial constitutional democracy, bhakti is partly integrated into mainstream Hindu institutional practice (temples admit all devotees regardless of caste in most modern urban settings) while continuing to justify alternative guru lineages and informal devotional groups. The arrangement has NOT become purely theatrical (the devotional experience is still genuine), but the institutional function has partly transitioned from revolutionary alternative to accepted variant. This is NOT a mandatrophy signal — the founding problem is still live, the coordination is still real, the resistance is still present — but it is a maturation signal: the reading has achieved institutional accommodation without fully displacing the hereditary order. The theater ratio at 0.25 is consistent with this: some performance of boundary/distinctiveness (to mark the reading as alternative) coexists with genuine devotional practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    devotional_sincerity_verification,
    'How is sincere bhakti (devotion) verified or certified? Is the reading''s claim that sincere devotion alone determines spiritual authority actually free from gatekeeping, or do devotional communities develop their own certification mechanisms (guru recognition, initiation ritual, community-tested wisdom) that reconstitute hierarchy even within the egalitarian reading?',
    'Ethnographic study of how devotional communities actually certify teachers and spiritual authority; historical analysis of guru lineages and initiation requirements; comparison of gatekeeper density in hereditary vs. devotional institutions.',
    'If devotional communities do develop re-entry certification, the reading is less purely egalitarian and the extractiveness may be higher than 0.40 (the cost of proving sincerity might be as onerous as the cost of birth privilege for those without cultural fluency). If sincere devotion is truly self-verifying through internal experience, extractiveness remains moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotional_sincerity_verification, empirical, 'Whether sincere devotion is genuinely self-certifying or whether devotional authority structures reproduce gatekeeping at a different layer.').

omega_variable(
    devotional_reach_across_caste,
    'To what extent can lower-caste and marginalized populations actually ACCESS bhakti practice and authority development, given constraints of time, literacy, cultural knowledge, gender restriction, and occupational labor demands that the non-devotional caste hierarchy imposes? Is bhakti access itself materially constrained in ways that limit its egalitarian function?',
    'Historical and contemporary data on caste and gender composition of devotional communities; time-budget studies of lower-caste participation in devotional practice vs. occupational labor; literacy rates required for engagement with devotional texts.',
    'If access is substantially constrained by material caste hierarchy, the reading''s beneficiary set shrinks (only those with leisure/literacy can truly benefit) and lower-caste aspirants'' role changes from beneficiary to partially-excluded, raising extractiveness. If access is broadly open despite material constraints, the reading delivers coordination across wider population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(devotional_reach_across_caste, empirical, 'Whether bhakti reading delivers spiritual authority access to lower castes or whether material constraints limit effective participation.').

omega_variable(
    hermetic_vs_hybrid_devotional_identity,
    'Can a lower-caste devotee simultaneously hold authority in the devotional sphere (through sincere practice) and remain subordinate in the non-devotional material economy (restricted occupation, limited land access, ritual pollution stigma)? Is this hybrid identity sustainable, or does achievement of spiritual authority eventually pressure for material equality?',
    'Historical-sociological study of whether devotional authority achieved by lower-caste teachers eventually enabled occupational mobility, marriage-rule changes, or property access. Ethnographic analysis of contemporary devotional leaders'' experience of caste restriction outside the devotional context.',
    'If the hybrid identity is stable and sustainable, the reading is pure coordination that redistributes authority without eliminating material hierarchy (extractiveness stable at 0.40, theater rises slightly as boundary performance intensifies). If the reading eventually pressures toward full material equality, it transforms into a reformist lever (extractiveness rises as the constraint becomes transformative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermetic_vs_hybrid_devotional_identity, conceptual, 'Whether spiritual authority egalitarianism can coexist indefinitely with material caste hierarchy or whether it eventually pressures toward material equality.').

omega_variable(
    kernel_committer_framing_ambiguity,
    'Is the Vedic-Dharmic corpus ''genuinely'' open to multiple readings as this constraint assumes, or does the hereditary reading claim exclusive interpretive authority such that the bhakti and reformist readings are ''misreadings''? The constraint''s persistence depends partly on this ontological question: are three readings three equally-valid approaches to one text, or is one reading the ''true'' reading and the others contestation?',
    'Genealogy of how the hereditary, bhakti, and reformist readings were instituted and challenged in commentary traditions and modern institutional practice. Analysis of whether any reading claims exclusive hermeneutic authority or whether all three acknowledge the others'' legitimacy.',
    'If one reading claims and enforces exclusive authority (likely the hereditary reading in its strictest form), the kernel is not genuinely open and the bhakti reading is a CONSTRAINED ALTERNATIVE (extractiveness may be higher, more suppression required). If all three coexist as acknowledged alternatives, the kernel is genuinely open and the reading system is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_framing_ambiguity, conceptual, 'Whether the Vedic-Dharmic corpus is genuinely open to multiple readings or whether the kernel''s authority structure dictates one reading as canonical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t5, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(vedi_tr_t5, observed).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(vedi_tr_t10, observed).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t15, projected).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(vedi_tr_t20, projected).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t5, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(vedi_be_t5, observed).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(vedi_be_t10, observed).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(vedi_be_t15, projected).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(vedi_be_t20, projected).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(vedi_be_t30, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_dharmic_corpus__bhakti_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__bhakti_devotional_reading, 0.12).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The bhakti devotional reading is one of three constraint stories instantiating different readings of the Vedic-Dharmic corpus kernel. The three readings are structurally distinct constraints with different epsilon values, beneficiary/victim sets, and type classifications. Bhakti is a ROPE (coordination-primary, asymmetric but not pure extraction). Hereditary monopoly is a PITON (traditional authority persisting by institutional inertia). Reformist egalitarian is a TANGLED ROPE or SNARE (constitutional authority override, coercive against traditional institutions). Each reading is a live interpretive stance held by different parties in contemporary Hindu society; no single reading has achieved complete dominance, and the kernel itself remains contested. Link all three stories via affects_constraints to represent the interpretive family and enable cross-reading analysis of how each reading pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
