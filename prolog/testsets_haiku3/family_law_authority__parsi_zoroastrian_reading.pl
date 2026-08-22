% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law and Community Endogamy
 *   domain: religious/familial/legal
 *
 * SUMMARY:
 *   Parsi Zoroastrianism is a small, historically persecuted religious
 *   community (Persian origin, diaspora in India and global refugee centers).
 *   The Parsi reading of the family law kernel privileges religious endogamy
 *   and priestly authority over marriage validity as a mechanism of community
 *   preservation. This reading instantiates a tangled rope: genuine
 *   coordination function (preserving a small community's religious identity
 *   and practice against assimilationist pressures) combined with asymmetric
 *   extraction (individuals in interfaith partnerships lose community status
 *   and belonging; non-Zoroastrian spouses are structurally excluded). The
 *   constraint is maintained by active institutional gatekeeping (fire
 *   temples, ceremonies, social recognition) and enforced through
 *   identity-based social exclusion rather than legal coercion. This reading
 *   coexists with secular, Hindu, Muslim, and Christian readings of the same
 *   family law kernel; each reading produces a different constraint with
 *   different stakeholders, different beneficiary/victim structures, and
 *   different ε values. The constraint is NOT presented as natural law—it is
 *   explicitly framed by the Parsi authority as a doctrinal requirement
 *   grounded in religious tradition and community survival necessity.
 *
 * KEY AGENTS:
 *   - Parsi priestly authority (agenda-setter): controls marriage legitimacy and access to religious ceremonies; maintains interpretive authority over Zoroastrian law
 *   - Parsi individuals in interfaith partnerships (target): bear identity fragmentation and ceremonial exclusion; identity-locked to the community they are excluded from
 *   - Non-Zoroastrian spouses (victims): excluded from community institutions and ritual participation; constrained by spouse's divided loyalty
 *   - Parsi community as collective (beneficiary): gains community continuity and identity preservation; maintains bounded reproduction and cultural transmission
 *   - Younger diaspora Parsis (dual-positioned): benefit from cultural cohesion but constrained by marriage gatekeeping in assimilationist contexts
 *   - Parsi secular reformers (excluded): would reframe marriage validity as decoupled from endogamy but are structurally barred from religious authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.71).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law and Community Endogamy").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious/familial/legal").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'a28b751b-9cec-4938-bd8c-2519728452b0').
narrative_ontology:cs_kernel_codification('a28b751b-9cec-4938-bd8c-2519728452b0', fixed_text).
narrative_ontology:cs_authority_grounding('a28b751b-9cec-4938-bd8c-2519728452b0', lineage).
narrative_ontology:cs_interpretation_layer_present('a28b751b-9cec-4938-bd8c-2519728452b0').
narrative_ontology:cs_reading_relation('a28b751b-9cec-4938-bd8c-2519728452b0', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28b751b-9cec-4938-bd8c-2519728452b0', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28b751b-9cec-4938-bd8c-2519728452b0', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28b751b-9cec-4938-bd8c-2519728452b0', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('a28b751b-9cec-4938-bd8c-2519728452b0', foundational, endogamy_doctrine_religious_requirement).
narrative_ontology:cs_axiom_status(endogamy_doctrine_religious_requirement, holdable).
narrative_ontology:cs_axiom_grounding('a28b751b-9cec-4938-bd8c-2519728452b0', endogamy_doctrine_religious_requirement, deontological).
narrative_ontology:cs_axiom('a28b751b-9cec-4938-bd8c-2519728452b0', foundational, priestly_authority_marriage_legitimacy).
narrative_ontology:cs_axiom_status(priestly_authority_marriage_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a28b751b-9cec-4938-bd8c-2519728452b0', priestly_authority_marriage_legitimacy, conventional).
narrative_ontology:cs_axiom('a28b751b-9cec-4938-bd8c-2519728452b0', secondary, community_preservation_transcends_individual_autonomy).
narrative_ontology:cs_axiom_status(community_preservation_transcends_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a28b751b-9cec-4938-bd8c-2519728452b0', community_preservation_transcends_individual_autonomy, instrumental).
narrative_ontology:cs_reference_frame('a28b751b-9cec-4938-bd8c-2519728452b0', zoroastrian_endogamous_doctrine).
narrative_ontology:cs_drift_state('a28b751b-9cec-4938-bd8c-2519728452b0', contemporary_diaspora_assimilation_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a28b751b-9cec-4938-bd8c-2519728452b0', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_as_collective).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_in_interfaith_partnerships).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_zoroastrian_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_in_interfaith_partnerships).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, younger_diaspora_parsis).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, younger_diaspora_parsis).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_survival_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, ritual_purity_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Zoroastrian priesthood and community councils (panchayats) adjudicate marriage validity, interpret religious law, and administer ritual permissions. They control whether a marriage is recognized within the community, access to fire temples, and participation in religious ceremonies. They justify authority as guardians of Zoroastrian continuity and doctrinal purity, and they collect deference and authority over familial legitimacy.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Parsi individuals who marry outside the faith face loss of community status, exclusion from fire temple privileges, and social ostracism. Some maintain both religious and family identities but at the cost of living partially outside community structures. They bear the cost of the endogamy requirement through identity fragmentation, ceremonial exclusion, and family fracture. They also benefit from the stability and identity continuity the constraint provides to those who comply.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_in_interfaith_partnerships, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_in_interfaith_partnerships, beneficiary).

% Non-Zoroastrian spouses of Parsi partners are barred from fire temple entry, cannot participate in major religious ceremonies, and are treated as outside the community even after marriage. Their children's religious status is contested. They bear the cost of structural exclusion and their spouse's divided loyalty without access to the community's institutions or legitimacy.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_zoroastrian_spouses, payer,
    moderate, biographical, constrained, global).

% The Parsi community as a whole benefits from endogamy rules as a mechanism of community preservation—maintaining distinct identity, language, religious practice, and cultural transmission across diaspora. The constraint ensures that Parsiism remains a bounded, reproducing community rather than dissolving into larger majority populations. Community members carry a collective interest in survival that exceeds individual preference; the constraint codifies this collective priority.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_as_collective, beneficiary,
    organized, civilizational, identity_locked, global).

% Some Parsis advocate for opening marriage ceremonies to interfaith couples and decoupling religious status from marriage validity. They are structurally excluded from authority over religious law (priesthood gates exclude them) and their voices are treated as heretical or as betraying community survival. Their position would fundamentally alter the constraint if accepted, but priesthood authority prevents its formal consideration.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_secular_reformers, excluded,
    moderate, biographical, constrained, regional).

% State law in India and diaspora jurisdictions permits interfaith marriage and treats religious exclusion as discretionary community practice. The state does not enforce endogamy but also does not compel religious communities to alter admission standards. It observes the constraint but leaves adjudication to community institutions.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_state_authority, observer,
    institutional, generational, analytical, national).

% Young Parsis in Western diaspora benefit from the community cohesion the constraint provides but face the constraint as an exit barrier: pursuing interfaith relationships means loss of family acceptance and community belonging. They experience the constraint as both protective (of their cultural identity) and coercive (limiting their autonomy in spouse choice).
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, younger_diaspora_parsis, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, younger_diaspora_parsis, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Parsi community continuity and religious distinctiveness across diaspora and assimilationist pressures by requiring marriage within the faith. Solves the collective-action problem of preserving a small, historically persecuted community facing numerical decline and cultural dissolution in majority-culture contexts.
% TRANSFER_FUNCTION: Transfers autonomy in spouse selection from individual Parsi actors to community/priestly authority; transfers ritual legitimacy and social belonging exclusively to those who comply with endogamy norms. The constraint moves freedom-to-marry from Parsi individuals and their non-Zoroastrian partners to the collective preservation of the Parsi identity-project.
% ABSENT_VOICES: Young Parsis who would choose interfaith marriage; non-Zoroastrian spouses who have married Parsis and been excluded; Parsi secular reformers advocating for religious pluralism. These voices would argue for decoupling marriage validity from faith endogamy and for recognizing children of interfaith Parsi couples as community members.
% DISAPPEARANCE_RATIONALE: If the endogamy requirement and priestly gatekeeping vanished, Parsi marriage patterns would diversify, interfaith families would integrate into community institutions, children of mixed unions would be recognized as Parsi by religious law, and some Parsis would exit endogamous marriage norms within one generation. The constraint is not a natural law—it is maintained by active institutional exclusion—so its removal would reshape community demographics and institutional authority.
% FOUNDING_PROBLEM: Zoroastrianism is a small, historically persecuted community (persecution by Arab conquest, Mughal rule, assimilation pressures in diaspora). Endogamy serves as a survival mechanism: keeping the community religiously and socially distinct preserves Zoroastrian practice, language, and identity transmission when the numerical and cultural pressures of majority populations would otherwise dissolve the community within a few generations.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community historians and scholars attest that numerical decline is a genuine historical fact (from ~900,000 in 1960s India to ~57,000 today) and that interfaith marriage correlates with reduced religious practice and community participation among offspring. Secular demographers note the documented diaspora pattern. However, some community scholars dispute whether endogamy is a necessary or proportionate response—arguing that religious transmission, institutional vitality, and cultural practice can be sustained without marriage gatekeeping. The status remains contested in reformist Parsi circles.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the constraint transfers marriage autonomy from individuals to institutional authority and uses community status as enforcement mechanism. The measurement series shows modest accumulation (0.55 → 0.68) over the interval, reflecting increasing diaspora pressure and tightening gatekeeping as numerical decline accelerates. Suppression (0.71) is high because the constraint persists primarily through social exclusion and identity-loss threat rather than legal force; individuals internalize the constraint through socialization and identity fusion. Theater (0.28) is moderate-low: the stated function (community preservation) is genuine and operational, but some enforcement activity defends priestly authority over marriage legitimacy specifically, not just community cohesion. Accessibility collapse (0.62) reflects that alternatives (interfaith marriage, secular marriage, religious marriage outside priesthood) exist but carry substantial identity cost. Resistance (0.58) is moderate: younger Parsis and some reformers resist the constraint, and intermarriage rates are rising despite community disapproval, indicating the constraint faces meaningful resistance even as it persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint looks like coordination from inside the priesthood and from the majority of community members who comply with endogamy norms. It looks like extraction from Parsi individuals in interfaith partnerships (who are targeted) and from non-Zoroastrian spouses (who are excluded entirely). Younger Parsis experience both poles: the constraint provides identity continuity they value, but also constrains their autonomy in marriage choice. The engine's per-seat computation captures this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi priestly authority is the structural beneficiary (d ≈ 0.1): they control the rule-set, collect deference and authority, and maintain their institutional position through the constraint. Parsi community-as-collective is a beneficiary (d ≈ 0.2): they receive community continuity and identity preservation. Parsi individuals in interfaith partnerships sit near the target end (d ≈ 0.85): they bear the full identity cost, are excluded from community participation, and have no exit that preserves both their Parsi identity and their marriage. Non-Zoroastrian spouses are pure targets (d ≈ 1.0): they are structurally barred from the community, experience no benefit, and have constrained exit (maintain marriage but accept spousal isolation, or dissolve marriage). Younger diaspora Parsis are harder to place (d ≈ 0.55): they benefit from community identity but are constrained by gatekeeping and bear costs if they choose interfaith partnerships. The constraint's directionality is NOT symmetric—it extracts from those it excludes to benefit those it includes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (community survival against assimilation) is LIVE, not dead. The constraint continues to serve this function—Parsi numerical decline is documented, and endogamy remains a stated mechanism for preserving religious practice and community identity. However, the mandatrophy question arises in the FORM of the constraint, not its necessity: is marriage gatekeeping specifically the optimal mechanism for solving the community-survival problem, or is it an extraction mechanism dressed in community-survival language? The reformist reading argues that community preservation could be maintained through religious education, institutional vitality, and cultural transmission without excluding interfaith families. This reading would reframe the constraint as a ratchet—the community adopted endogamy as necessary defense during persecution (live problem), but now persists in enforcing it even as legal persecution has ended and institutional alternatives exist (mandatrophy emerging). The constraint is not yet a piton (it is actively defended by priestly authority and complied with by most community members), but it carries mandatrophy risk if the founding problem becomes materially dead while the enforcement structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression primarily structural (legal/institutional barriers, concrete exclusions) or internalized (Parsi individuals police their own marriage choices through identity fusion and internalized shame)?',
    'Ethnographic observation of Parsi individuals who have exited the community constraint (through interfaith marriage) and trace their suppression trajectory: if suppression persists post-exit (self-judgment, identity-fracture), it is substantially internalized; if suppression drops upon institutional exit, it is primarily structural.',
    'If internalized, the constraint''s effective suppression exceeds the institutional measure—individuals carry the constraint''s enforcement mechanism within themselves even after formal institutional barriers are removed. This would shift classification toward snare (the suppression persists through psychological capture). If structural, the constraint''s suppression is contingent on active institutional enforcement and could be reversed through institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Distinction between internalized (identity fusion) and structural (institutional gatekeeping) suppression mechanisms in Parsi endogamy enforcement.').

omega_variable(
    community_survival_necessity_vs_extraction_cover,
    'Is endogamy a necessary mechanism for Parsi community survival, or is it a rent-extraction mechanism by priestly authority dressed in community-survival language? Could the community preserve its religious practice and identity through institutional vitality, cultural education, and voluntary participation without excluding interfaith families?',
    'Comparative analysis of other small religious communities (Jews, Jains, Copts) that maintain distinct religious practice without enforcing marriage endogamy; examination of Parsi community vitality in regions where endogamy has relaxed; demographic analysis of whether children of Parsi-interfaith couples show reduced religious participation or whether reduced participation is confounded with generational secularism across all communities.',
    'If endogamy proves unnecessary for community survival, the constraint is pure extraction riding on a real problem (community preservation) but using a disproportionate mechanism. If endogamy is empirically necessary, the constraint is authentic tangled rope (real coordination with asymmetric costs). This would affect mandatrophy assessment: necessary pain is justified; unnecessary pain is exploitative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_survival_necessity_vs_extraction_cover, empirical, 'Whether endogamy is a necessary mechanism for Parsi community preservation or an extraction mechanism using community survival as cover.').

omega_variable(
    reading_mutuality_and_foreclosure,
    'Does the Parsi Zoroastrian reading''s claim that marriage must be under priestly authority and endogamous FORECLOSE the secular contractual reading''s claim that marriage should be a civil contract between individuals? Or do these readings coexist as different parties'' frameworks without logical contradiction?',
    'Examine whether a Parsi individual can simultaneously hold (1) that within their own community, marriage legitimacy derives from priestly authority and endogamy, AND (2) that in secular law, marriage is a civil contract between individuals. If both can be held without internal contradiction—i.e., the readings address different jurisdictions or populations—they coexist. If holding one requires rejecting the core premise of the other—i.e., a single authority structure cannot ground both priestly gatekeeping and individual autonomy—they foreclose.',
    'If they coexist, the readings are different answers to the same kernel by different parties—a normal kernel contest. If they foreclose, one reading''s acceptance would require the other''s rejection within any unified framework, indicating deeper structural incompatibility in how legitimacy is grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_mutuality_and_foreclosure, conceptual, 'Whether the Parsi and secular readings coexist or foreclose each other.').

omega_variable(
    priestly_authority_identity_locked_vs_chosen,
    'To what extent is Parsi individuals'' acceptance of priestly marriage authority grounded in identity-locked commitment to the Parsi tradition (the authority is inseparable from their sense of self) versus voluntary deference to institutional expertise?',
    'Survey and ethnographic work differentiating reasons Parsi individuals comply with priestly gatekeeping: (a) ''This is who I am; challenging it would fracture my identity,'' (b) ''The priesthood has legitimate expertise in religious law I defer to,'' (c) ''The community would ostracize me if I defied the authority.'' High (a) indicates identity-lock; low (a) and high (b) indicates structural deference.',
    'Identity-lock deepens the constraint''s grip and makes exit more costly. It also suggests the constraint is enforced partly through psychological capture, not just institutional pressure. This would influence whether the constraint should be classified as having internalized suppression (omega 1) and whether reform is possible through institutional change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_authority_identity_locked_vs_chosen, empirical, 'Degree to which Parsi acceptance of priestly marriage authority is identity-locked versus chosen/deferred.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fami_be_t5, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(fami_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(fami_be_t25, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fami_su_t5, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fami_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fami_su_t25, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the family law authority kernel. Each reading instantiates the kernel differently: the Parsi reading grounds legitimacy in priestly authority and community preservation; the secular reading grounds it in individual autonomy and state law; the Christian, Hindu, and Muslim readings ground it in scriptural authority and religious tradition. All five readings are linked via network.affects_constraints because they compete for applicability to the same domain (marriage legitimacy). The constraint family's classification divergence is the point—each reading should produce a different constraint type reflecting its structural asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
