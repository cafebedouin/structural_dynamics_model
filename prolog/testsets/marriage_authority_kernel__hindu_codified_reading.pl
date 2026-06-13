% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority (Civil Court Interpretation)
 *   domain: constitutional/religious/family law
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 codified Hindu marriage law under the
 *   authority of Brahminical interpretive tradition, administered through
 *   civil courts. The Act presents itself as crystallizing eternal dharma
 *   into modern statutory form, granting women statutory recognition and
 *   judicially mediated divorce rights while maintaining patriarchal
 *   presumptions embedded in customary law (asymmetrical divorce grounds,
 *   marital obedience presumptions, patrilineal succession defaults). The
 *   constraint's extraction arises from the dual structure: it genuinely
 *   coordinates marriage law (solving fragmentation and inaccessibility)
 *   while simultaneously formalizing patriarchal and caste-stratified
 *   privilege as law. The measurement series (70 years, 1955–2025) shows
 *   declining explicit suppression as courts interpret the statute more
 *   liberally, but rising resistance and stable extraction, suggesting
 *   community enforcement is hardening against statutory reform rather than
 *   courts' liberalization representing genuine decentralization.
 *
 * KEY AGENTS:
 *   - Hindu institutional authority (temples, caste councils, community leaders): maintains interpretive tradition, frames the Act as dharma, opposes reform that breaks from 'community values'
 *   - Civil courts (state and constitutional judiciary): formal adjudicators, caught between statutory text, constitutional rights, and community expectations; increasingly broaden divorce grounds through creative interpretation
 *   - Upper-caste Hindu men: structural beneficiaries of default patriarchal rules, retain advantages in statutory grounds and property division
 *   - Upper-caste Hindu women: partial beneficiaries (statutory recognition, divorce access superior to custom) but constrained by restrictive grounds and community enforcement
 *   - Lower-caste Hindu women: powerless payers; legal protections are inaccessible due to cost, geography, and caste enforcement; trapped by identity-lock (community marriage is identity-constitutive; exit is social death)
 *   - Interfaith couples in Hindu framework: excluded from the constraint's scope yet captured by its consequences; cannot form families under Hindu law if one party converts or was born outside
 *   - Feminist reformers and constitutional advocates: excluded from the constraint's legitimacy frame, advocate for secular civil code, but their voice appears in constitutional review, not in the statute's reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.38).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Codified Marriage Authority (Civil Court Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "constitutional/religious/family law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'c6cce1ce-3a4d-42da-a291-4253f4254dea').
narrative_ontology:cs_kernel_codification('c6cce1ce-3a4d-42da-a291-4253f4254dea', fixed_text).
narrative_ontology:cs_authority_grounding('c6cce1ce-3a4d-42da-a291-4253f4254dea', lineage).
narrative_ontology:cs_interpretation_layer_present('c6cce1ce-3a4d-42da-a291-4253f4254dea').
narrative_ontology:cs_reading_relation('c6cce1ce-3a4d-42da-a291-4253f4254dea', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6cce1ce-3a4d-42da-a291-4253f4254dea', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6cce1ce-3a4d-42da-a291-4253f4254dea', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6cce1ce-3a4d-42da-a291-4253f4254dea', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('c6cce1ce-3a4d-42da-a291-4253f4254dea', foundational, brahminical_interpretive_tradition_as_authority_source).
narrative_ontology:cs_axiom_status(brahminical_interpretive_tradition_as_authority_source, holdable).
narrative_ontology:cs_axiom_grounding('c6cce1ce-3a4d-42da-a291-4253f4254dea', brahminical_interpretive_tradition_as_authority_source, conventional).
narrative_ontology:cs_axiom('c6cce1ce-3a4d-42da-a291-4253f4254dea', foundational, marriage_as_community_status_not_individual_contract).
narrative_ontology:cs_axiom_status(marriage_as_community_status_not_individual_contract, holdable).
narrative_ontology:cs_axiom_grounding('c6cce1ce-3a4d-42da-a291-4253f4254dea', marriage_as_community_status_not_individual_contract, deontological).
narrative_ontology:cs_axiom('c6cce1ce-3a4d-42da-a291-4253f4254dea', secondary, patriarchal_marital_defaults_embedded_in_dharma).
narrative_ontology:cs_axiom_status(patriarchal_marital_defaults_embedded_in_dharma, overridden).
narrative_ontology:cs_axiom_grounding('c6cce1ce-3a4d-42da-a291-4253f4254dea', patriarchal_marital_defaults_embedded_in_dharma, empirically_contingent).
narrative_ontology:cs_reference_frame('c6cce1ce-3a4d-42da-a291-4253f4254dea', brahminical_tradition_codified_into_statute).
narrative_ontology:cs_drift_state('c6cce1ce-3a4d-42da-a291-4253f4254dea', contemporary_constitutional_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c6cce1ce-3a4d-42da-a291-4253f4254dea', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_institutional_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, brahminical_interpretive_lineage).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples_in_hindu_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_men).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_women).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, constitutional_minority_rights_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, personal_law_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hindu organizations, temple administrations, and community councils operate as custodians of marriage norms and interpretation. They frame the Hindu Marriage Act 1955 as codification of eternal dharma, not as a constructive human law. They set expectation boundaries on conduct, inheritance, and divorce grounds, and influence (though do not formally adjudicate) the civil courts' reading of the statute.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_institutional_authority, agenda_setter,
    institutional, generational, constrained, national).

% State civil courts formally adjudicate marriage disputes under the Hindu Marriage Act and interpret its provisions. They hold the structural authority to construe statutory terms (cruelty, desertion, adultery) and to award remedies. They benefit from having a codified framework within which to work; they also face pressure to align with community expectations and constitutional fundamentals simultaneously.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, civil_courts_judiciary, beneficiary).

% Retain customary authority within households (the constraint codifies patrilineal inheritance, male guardianship defaults, restrictive divorce grounds for women). The Act's grounds for marriage dissolution are asymmetrical: men can exit on broader grounds (adultery, desertion, cruelty defined narrowly). They benefit from statutory codification of customary privilege.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_men, beneficiary,
    powerful, generational, constrained, national).

% Subject to restrictive marriage dissolution rules, limited inheritance rights within joint family structures, and assumption of marital domicile and obedience. They receive statutory recognition of marriage as a status (better than custom-only) and limited grounds for divorce (cruelty, desertion, mental illness of spouse). Exit requires court dissolution, not consensual separation, and still carries social cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_women, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, upper_caste_hindu_women, beneficiary).

% Bear the constraint's costs without meaningful access to its protections: court remedies are expensive and geographically distant; community enforcement of patriarchy is severe; caste-stratified divorce grounds mean their marriages dissolve less readily even under identical cruelty; they are trapped by economic dependence, caste endogamy norms, and absence of safe refuge structures.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindu_women, payer,
    powerless, biographical, identity_locked, national).

% A Hindu-married partner converting or marrying outside the religion faces statutory termination of marriage or forced legal reclassification under Special Marriage Act; the codified Hindu law gives no path for genuine pluralistic family formation. They are excluded from the Hindu Marriage Act's scope yet captured by its consequences.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples_in_hindu_framework, excluded,
    moderate, biographical, trapped, national).

% Advocate for gender-equal marriage law and secular civil code, but are structurally excluded from the Hindu Marriage Act's legitimacy frame (which grounds authority in community tradition, not individual rights). Their voice appears in legislative debate but not in the reading of the statute itself, which treats women's marriage dissolution rights as a deviation from normal principle, not as rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, feminist_reform_movements, excluded,
    organized, biographical, constrained, national).

% Review the constitutionality of the Hindu Marriage Act and its interpretation against Articles 14 (equality), 15 (no discrimination on sex), and 25 (freedom of religion). They provide a second layer of adjudication but rarely overturn the substantive grounds themselves, instead moderating application (e.g., broadening cruelty definition, requiring substantive proof of desertion).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, hindu_community_institutional_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform, codified marriage law for Hindu-married persons in place of fragmented customary law varying by caste, region, and family practice. Offers statutory grounds for marriage dissolution and property settlement, replacing entirely customary-law processes. Creates a common interpretive framework across state lines within India.
% TRANSFER_FUNCTION: Transfers authority to adjudicate marriage disputes from family councils and community arbiters to state civil courts. Transfers interpretive power from Brahminical textual tradition (in private control) to a public statutory framework. Simultaneously transfers marital exit rights from men (who could unilaterally repudiate in custom) to courts (who now mediate). But the direction is asymmetric: women gain the ability to petition; men retain broader grounds and structural advantage. The constraint moves authority upward (to courts) and sideways (to organized institutional tradition) while appearing to codify eternal principles rather than making new choices.
% ABSENT_VOICES: Lower-caste Hindus whose marriage customs predate and differ from Brahminical dharma; Dalit women whose critiques of caste patriarchy are invisible to a reading that treats 'Hindu marriage' as unified; interfaith partners whose family formations are constitutively excluded; secular critics who view the constraint as religious establishment rather than personal law. These voices appear in comparative constitutional debate but not in the constraint's own interpretive frame, which treats it as community-internal codification, not as contested jurisdiction.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its enforcement disappeared overnight, marriage disputes within Hindu communities would either revert to customary law (caste councils, family arbitration, religious gurus), migrate to the secular Special Marriage Act, or remain entirely privatized and unmediated. The institutional capacity to enforce uniform marital dissolution and property distribution would evaporate. Tens of thousands of annual divorce cases and property disputes would lose their formal venue. The constraint's removal is politically unthinkable to the beneficiary institutional actors, which is exactly why its persistence depends on active enforcement of the reading—courts constantly re-interpret the statute to block exits they would permit under updated standards.
% FOUNDING_PROBLEM: Pre-1955, Hindu marriage law was customary, fragmented by caste and region, inaccessible to poor litigants, and gave women almost no route to marriage dissolution or property rights. The Act was built to solve this: create uniform statutory grounds for marriage, give women judicially mediated divorce access, establish marital property division, and reduce the private power of family councils.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional reform advocates and feminist scholars attest the founding problem was real and the Act made progress on women's property rights and divorce access. However, they also attest the Act did not solve the problem fully—the statute's grounds remain restrictive compared to secular civil law, enforcement is weak for lower-caste women, and community pressure often prevents divorce access from translating into actual exit. Community leaders attest the founding problem is still live (customary law persists, caste councils still arbitrate). Courts' own jurisprudence shows increasing discomfort with the statute's rigidity: they have progressively broadened 'cruelty' beyond physical harm to include mental cruelty and gender-based harassment, effectively rewriting the statute to fit modern rights norms—which simultaneously signals that the original founding purpose (uniform accessible law) is being met but the original premises (caste patriarchy as default) are decomposing.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 endpoint) because the constraint serves a genuine coordination function (uniform marriage law, access to courts) that would collapse if removed, but the extraction is real: patriarchal defaults, caste-stratified enforcement, and exclusion of lower-caste and interfaith actors from benefit. Suppression is moderate-to-low (0.38 endpoint) because formal court enforcement is weak (many disputes never reach courts) and community enforcement relies on internalized norms and endogamy rather than external coercion. Theater ratio is low (0.22 endpoint) because the constraint does substantive work: courts actually adjudicate disputes, property is divided, divorce is granted. The rising theater over time (0.08 → 0.22) reflects increasing performative dimension: courts broaden grounds through reinterpretation (appearing reformist) while community enforcement hardens against exit (maintaining extraction), creating a dual track where formal law and social control diverge. The grid shows asymmetric leveled coercion: individual women face high stakes inflation (0.74 at t0, 0.68 at t70) even as organizational suppression declines (judicial liberalization), indicating the constraint's extraction has migrated from explicit enforcement to internalized identity-lock.
 *
 * PERSPECTIVAL GAP:
 *   A beneficiary seat and a powerless-victim seat should compute different types: from the beneficiary institutional perspective, this is tangled rope (genuine coordination + organized institutional benefit); from the powerless victim perspective, it resembles snare (extraction with identity-lock preventing exit). The engine computes per-seat, so the claimed type (tangled rope) represents the institutional-seat perspective, while the victim seats' computations may diverge. This divergence is the measurement the corpus is built to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper-caste institutional beneficiaries and the Brahminical tradition sit at d ≈ 0.15–0.25 (beneficiary end): they set the agenda, interpret the statute, and benefit from its codification of customary privilege. Civil courts sit at d ≈ 0.50 (symmetric): they face pressure from both institutional tradition and constitutional rights, and their authority is genuine (they actually adjudicate) but mediated by competing frameworks. Upper-caste women sit at d ≈ 0.40 (slightly toward target): they benefit from statutory recognition and divorce access superior to custom but are constrained by restrictive grounds and property law; their exit is difficult but not identity-locked. Lower-caste women sit at d ≈ 0.75–0.85 (target end): they bear the constraint's cost (lack of meaningful access to courts, caste enforcement prevents exit even if legal grounds existed) without proportional benefit; their identity-lock (community marriage is caste-constitutive) makes exit unthinkable. Interfaith couples sit at d ≈ 0.80 (target end): excluded from the statute's scope but captured by its consequences (marital status indeterminacy, property uncertainty). These directionalities are derived from the structural data (beneficiary/victim + power + exit) without override; the constraint's extraction concentrates on powerless, identity-locked actors, which is precisely the engine's intended measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (uniform accessible marriage law, women's divorce access) is partially solved: courts do adjudicate disputes and statutory grounds do exist. However, the six-questions data show the founding problem status is 'contested'—community leaders say the problem is still live (customary law persists alongside statute, caste councils still arbitrate), while feminist reformers say it is 'dead' (the problem was solvable by secular individual-rights law, not by reforming personal law). The disappearance verdict is 'world_rearranges' (the constraint's removal would force reversion to customary arbitration or migration to secular law). The measurement series show extraction stable but theater rising, which signals mandatrophy resolution: the constraint's original coordination function (access to marriage law) is being substituted by a performative function (courts appear to reform while community enforcement hardens). However, the constraint's persistence is still driven by institutional interest (courts exist to adjudicate, community leaders exist to maintain tradition), not by pure inertia. The constraint exhibits some piton symptoms (rising theater, declining formal suppression) but the institutional actors still collect from it (courts gain jurisdiction, Brahminical tradition gains state enforcement), so it is better classified as tangled rope where the coordination function is weakening but the extraction machinery is intact and the beneficiary institutional actors maintain it actively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_vs_constructed_authority,
    'Is the Hindu Marriage Act a codification of pre-existing eternal dharma, or a constructed modern statute that uses the dharma framing as legitimation?',
    'Historical and comparative analysis: examination of the drafting records (Nehru-era debates), comparison with pre-1955 customary law variations across castes and regions, analysis of gaps between dharmaśāstra prescription and the Act''s actual text. The Act''s provisions (permitting women''s divorce, restricting mens'' unilateral repudiation) diverge from classical Brahminical dharma—these divergences signal construction, not codification.',
    'If codification, the constraint derives legitimacy from community tradition and is appropriately administered through courts sensitive to tradition; if constructed, the constraint derives legitimacy from constitutional authority and democratic process, and should be administered against a baseline of individual rights, not community continuity. The dispute determines whether divergence from dharma signals failed application or successful reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(codification_vs_constructed_authority, conceptual, 'Whether the Hindu Marriage Act represents discovery of eternal law or construction of modern statute.').

omega_variable(
    brahminical_vs_dalit_marriage_traditions,
    'Does the Act genuinely codify ''Hindu marriage law'' or specifically Brahminical marriage law, and are lower-caste traditions erased or merely subordinated?',
    'Ethnographic and historical reconstruction of pre-1955 marriage law and ritual practice among Dalit, Shudra, and OBC communities; analysis of the Act''s interpretive tradition for references to non-Brahminical practice; examination of case law to identify disparities in application across caste groups.',
    'If the Act codifies Brahminical law as the default, it instrumentalizes the constraint as Brahminical cultural dominance even while framing it as plural community law. This would reclassify the constraint''s coordination function as narrower (Brahminical uniformity, not Hindu uniformity) and its extraction as caste-stratified. Dalit communities'' exclusion from the tradition''s legitimacy construction becomes a fundamental feature, not an accident.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(brahminical_vs_dalit_marriage_traditions, empirical, 'Whether the constraint codifies all Hindu marriage traditions or only Brahminical ones.').

omega_variable(
    gender_equity_vector_decay,
    'Why does the constraint show declining suppression and theater over 70 years (reformist interpretation broadening cruelty, recognizing mental abuse, loosening desertion proof) yet resistance continues rising and extractiveness stabilizes instead of collapsing?',
    'Longitudinal case-law analysis mapping judicial broadening of divorce grounds against divorce rates, approval rates, and pendency times across gender and caste; interviews with lower-caste women about exit barriers and why improved legal grounds do not translate to actual exit; analysis of community enforcement of endogamy and marital dissolution stigma alongside statutory reform.',
    'If resistance rises because community enforcement is hardening against statutory reform (while suppression falls because courts are loosening), the constraint remains extractive even as formal law modernizes. The theater ratio would indicate performative reform—courts broaden grounds while community enforcement prevents actual exit, maintaining both appearance of reform and substance of control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_vector_decay, empirical, 'Why formal legal reform does not reduce the constraint''s extraction.').

omega_variable(
    interfaith_family_exclusion_structural,
    'Is the exclusion of interfaith Hindu-married couples from the Act''s scope a technical jurisdictional boundary or a foundational commitment of the Hindu reading?',
    'Analysis of statutory text, case law on conversion and marital status, and comparison with how other pluralist readings handle interfaith families. If the Act''s authority depends on Hindu-ness of both parties, the constraint''s scope is inherently ethno-religious. If the exclusion is accidental, amendment could include interfaith families; if foundational, any inclusive reading would require shifting the authority base entirely.',
    'If foundational, the Hindu codified reading is ethno-religious communal law disguised as pluralist personal law—it cannot be reformed to include interfaith families without ceasing to be the Hindu reading. This would reclassify the constraint as a snare for interfaith couples (excluded from marital remedies) and the institutional beneficiary as the Hindu-only institutional authority. If accidental, it is a fixable gap in an otherwise pluralist framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_family_exclusion_structural, conceptual, 'Whether interfaith exclusion is structural to the Hindu reading or a contingent boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 35, 0.21).
narrative_ontology:measurement_basis(marr_tr_t35, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 50, 0.23).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement_basis(marr_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t35, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 35, 0.4).
narrative_ontology:measurement_basis(marr_be_t35, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.42).
narrative_ontology:measurement_basis(marr_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t35, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(marr_su_t35, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.38).
narrative_ontology:measurement_basis(marr_su_t70, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=70
narrative_ontology:measurement(marr_grid_01, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(marr_grid_02, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(class), 70, 0.64).
narrative_ontology:measurement(marr_grid_03, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(marr_grid_04, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(individual), 70, 0.58).
narrative_ontology:measurement(marr_grid_05, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(marr_grid_06, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(organizational), 70, 0.62).
narrative_ontology:measurement(marr_grid_07, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(marr_grid_08, marriage_authority_kernel__hindu_codified_reading, accessibility_collapse(structural), 70, 0.65).
narrative_ontology:measurement(marr_grid_09, marriage_authority_kernel__hindu_codified_reading, resistance(class), 0, 0.48).
narrative_ontology:measurement(marr_grid_10, marriage_authority_kernel__hindu_codified_reading, resistance(class), 70, 0.58).
narrative_ontology:measurement(marr_grid_11, marriage_authority_kernel__hindu_codified_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(marr_grid_12, marriage_authority_kernel__hindu_codified_reading, resistance(individual), 70, 0.62).
narrative_ontology:measurement(marr_grid_13, marriage_authority_kernel__hindu_codified_reading, resistance(organizational), 0, 0.38).
narrative_ontology:measurement(marr_grid_14, marriage_authority_kernel__hindu_codified_reading, resistance(organizational), 70, 0.51).
narrative_ontology:measurement(marr_grid_15, marriage_authority_kernel__hindu_codified_reading, resistance(structural), 0, 0.28).
narrative_ontology:measurement(marr_grid_16, marriage_authority_kernel__hindu_codified_reading, resistance(structural), 70, 0.42).
narrative_ontology:measurement(marr_grid_17, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(marr_grid_18, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(class), 70, 0.65).
narrative_ontology:measurement(marr_grid_19, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(individual), 0, 0.74).
narrative_ontology:measurement(marr_grid_20, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(individual), 70, 0.68).
narrative_ontology:measurement(marr_grid_21, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(marr_grid_22, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(organizational), 70, 0.55).
narrative_ontology:measurement(marr_grid_23, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(marr_grid_24, marriage_authority_kernel__hindu_codified_reading, stakes_inflation(structural), 70, 0.52).
narrative_ontology:measurement(marr_grid_25, marriage_authority_kernel__hindu_codified_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement(marr_grid_26, marriage_authority_kernel__hindu_codified_reading, suppression(class), 70, 0.35).
narrative_ontology:measurement(marr_grid_27, marriage_authority_kernel__hindu_codified_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(marr_grid_28, marriage_authority_kernel__hindu_codified_reading, suppression(individual), 70, 0.32).
narrative_ontology:measurement(marr_grid_29, marriage_authority_kernel__hindu_codified_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(marr_grid_30, marriage_authority_kernel__hindu_codified_reading, suppression(organizational), 70, 0.48).
narrative_ontology:measurement(marr_grid_31, marriage_authority_kernel__hindu_codified_reading, suppression(structural), 0, 0.55).
narrative_ontology:measurement(marr_grid_32, marriage_authority_kernel__hindu_codified_reading, suppression(structural), 70, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, hindu_marriage_dowry_extraction).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, caste_marriage_endogamy_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'marriage_authority_kernel'. The Hindu codified reading grounds marriage authority in Brahminical interpretive tradition and civil court administration. Sibling readings (Muslim shariat, Christian canonical, Parsi communal, secular civil) instantiate alternative authority sources for the same domain (marriage law in India). Each reading has distinct ε (extractiveness), distinct beneficiaries, and distinct structural relationships to exit and power. The Hindu reading is neither more nor less 'correct' than siblings—it is one live institutional position held by a major religious community. Network links show how the Hindu reading creates structural pressure on the secular reading (which must exist as an alternative that individuals can choose, thereby constraining the Hindu reading's scope) and how both readings are under pressure from constitutional courts interpreting individual rights. The family is best understood as a commitment-system kernel with multiple live readings competing for institutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, powerless, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
