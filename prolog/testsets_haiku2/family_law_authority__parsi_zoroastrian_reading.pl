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
 *   human_readable: Marriage Authority Under Zoroastrian Religious Law (Parsi Reading)
 *   domain: religious_governance/family_law
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'family_law_authority' — specifically, the Parsi Zoroastrian reading. The
 *   constraint under examination is the standing arrangement that governs
 *   marriage within traditional Zoroastrian law: priestly authority over
 *   ritual validity, endogamy requirements, and inheritance rules that tie
 *   community membership to marriage within the faith. This reading claims
 *   marriage is a community-preserving institution whose legitimacy derives
 *   from Zoroastrian textual authority and continuous transmission from the
 *   Sassanid priestly tradition. Sibling readings (Hindu Dharmashastra,
 *   Muslim Shariat, Christian Canonical, Secular Contractual) instantiate
 *   different constraints with different beneficiaries, victims, extraction
 *   profiles, and ε values—they are NOT variants of this constraint, but
 *   structurally distinct constraints grounded in different kernels and
 *   authorities. This story examines ONLY the Parsi reading and its internal
 *   structure.
 *
 * KEY AGENTS:
 *   - Zoroastrian priestly authority (institutional agenda-setter, deriving legitimacy from textual tradition and community mandate)
 *   - Parsi community institution (collective beneficiary, reproduces identity through marriage governance)
 *   - Individuals intermarrying outside faith (victims, face status loss and identity-locked exit)
 *   - Women under ritual law (victims and dual-positioned: benefit from community membership, pay through authority over marriage/inheritance)
 *   - Non-Parsi marriage partners (excluded, can convert but only to subordinate membership status)
 *   - Reform-movement Parsi leadership (observers competing for authority, propose alternative readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Marriage Authority Under Zoroastrian Religious Law (Parsi Reading)").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'c35c6a27-9f28-4589-b635-600bf6b789b8').
narrative_ontology:cs_kernel_codification('c35c6a27-9f28-4589-b635-600bf6b789b8', fixed_text).
narrative_ontology:cs_authority_grounding('c35c6a27-9f28-4589-b635-600bf6b789b8', lineage).
narrative_ontology:cs_interpretation_layer_present('c35c6a27-9f28-4589-b635-600bf6b789b8').
narrative_ontology:cs_reading_relation('c35c6a27-9f28-4589-b635-600bf6b789b8', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c35c6a27-9f28-4589-b635-600bf6b789b8', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c35c6a27-9f28-4589-b635-600bf6b789b8', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('c35c6a27-9f28-4589-b635-600bf6b789b8', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('c35c6a27-9f28-4589-b635-600bf6b789b8', foundational, endogamy_essential_community_preservation).
narrative_ontology:cs_axiom_status(endogamy_essential_community_preservation, holdable).
narrative_ontology:cs_axiom_grounding('c35c6a27-9f28-4589-b635-600bf6b789b8', endogamy_essential_community_preservation, instrumental).
narrative_ontology:cs_axiom('c35c6a27-9f28-4589-b635-600bf6b789b8', foundational, priestly_authority_derives_from_textual_tradition).
narrative_ontology:cs_axiom_status(priestly_authority_derives_from_textual_tradition, holdable).
narrative_ontology:cs_axiom_grounding('c35c6a27-9f28-4589-b635-600bf6b789b8', priestly_authority_derives_from_textual_tradition, conventional).
narrative_ontology:cs_reference_frame('c35c6a27-9f28-4589-b635-600bf6b789b8', zoroastrian_priestly_continuity).
narrative_ontology:cs_drift_state('c35c6a27-9f28-4589-b635-600bf6b789b8', contemporary_diaspora_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c35c6a27-9f28-4589-b635-600bf6b789b8', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priestly_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_institution).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, individuals_intermarrying_outside_faith).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, women_constrained_by_ritual_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, women_constrained_by_ritual_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Zoroastrian marriage law (Khordeh Avesta provisions on valid marriage, ritual purity, and community membership). Controls ritual validity certification, determines who may marry within the faith, and administers the ritual performance that legitimates the union in religious terms. Claims authority derives from Zoroastrian textual tradition and continuous transmission from the Sassanid era. Maintains community boundaries through marriage law enforcement.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priestly_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% The collective Parsi community (ca. 60,000 worldwide, ca. 10,000 in diaspora). Benefits from marriage law that restricts reproduction within the faith, slowing assimilation and demographic erosion. Controls cultural reproduction through controlling who is admitted as a member via marriage. Administers fire temples, social welfare systems, and inheritance structures tied to marriage status.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_institution, beneficiary,
    institutional, civilizational, mobile, global).

% Parsi individuals (and their non-Parsi partners) who wish to marry outside the faith. Face loss of ritual recognition, social standing within the community, inheritance rights, and access to community welfare. In traditional formulations they are excommunicated or downgraded to 'anuszowan' (adoptee) status without full membership. Their choice: forgo the partner, forgo community membership, or accept diminished status. The constraint persists because exit entails loss of identity as a practicing Zoroastrian.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, individuals_intermarrying_outside_faith, payer,
    moderate, biographical, identity_locked, regional).

% Parsi women whose marriage validity, divorce rights, custody of children, and inheritance are determined by priestly interpretation of ritual purity law. Historically, women's ritual status post-marriage and post-widowhood were governed by detailed rules of separation, ritual reintegration, and property control that assigned women reduced agency. Modern Parsi communities have reformed some provisions, but priestly authority over ritual legitimacy remains binding. Their exit is identity-locked: leaving the faith means losing cultural identity, family ties, and community belonging.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, women_constrained_by_ritual_authority, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, women_constrained_by_ritual_authority, beneficiary).

% Non-Zoroastrians who wish to marry Parsi individuals face categorical exclusion from the marriage ritual if they do not convert. If they convert, they are admitted as 'anuszowan' (adoptee members) with second-class ritual status in perpetuity—they can marry but their children are not automatically Parsi and they cannot serve as priests. Their only option for ritual recognition is formal conversion plus acceptance of subordinate membership.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_marriage_partners, excluded,
    moderate, biographical, constrained, regional).

% Parsi leaders and intellectuals advocating for reformed marriage law (e.g., acceptance of interfaith marriage, gender-equal inheritance, lay authority over ritual). They produce alternative readings of Zoroastrian texts that permit intermarriage while preserving faith identity. Their authority is intellectual and moral, not institutional; they compete for legitimacy with traditional priestly authority.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, reform_movement_parsi_leadership, observer,
    organized, biographical, analytical, global).

% State government (India, Iran, or diaspora nation-states) that recognizes or declines to recognize Zoroastrian marriage law as valid for civil purposes. Can mandate secular marriage registration, overturn religious law's inheritance effects, or recognize parallel systems. They are observers in the Parsi system but shape its external legal context.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, civil_state_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_community_institution).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains marriage ritual and inheritance rules that preserve Parsi religious and ethnic identity across generations. Coordinates collective action around endogamy: if each family independently assimilated (intermarriage), the community would dissolve; the law ensures marriages reinforce community continuity by restricting reproduction within the faith.
% TRANSFER_FUNCTION: Moves authority over marriage legitimacy, inheritance rights, social standing, and community membership from individuals and families to the priestly authority and community institution. Individuals seeking marriage within the faith must submit to priestly certification of ritual purity and community status; those intermarrying must forfeit ritual recognition and inherited rights. The community institution receives the power to control demographic reproduction.
% ABSENT_VOICES: Non-Parsi marriage partners and reform-minded Parsis are structurally excluded from the core authority structure that determines marriage validity. They can petition or propose alternative readings but cannot sit as equal participants in determining what marriage law is. Reform Parsis lack institutional authority despite articulating competing readings of Zoroastrian tradition.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, Parsi marriage law would cease to be enforceable by the priestly authority; individuals would marry across faith boundaries; the inheritance and community-membership consequences would no longer apply; the community would experience accelerated assimilation. The constraint is essential to the community's self-reproduction; its disappearance would structurally alter how Parsi identity is transmitted across generations.
% FOUNDING_PROBLEM: Zoroastrian minority communities (especially diaspora Parsis) faced assimilation pressure and demographic erosion. Marriage law governing endogamy and ritual purity was established to protect religious and cultural continuity: a small community cannot survive ethically if marriage partners are drawn from larger, culturally dominant neighboring populations. The law solved the collective-action problem of minority preservation.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Parsi priestly authority attests the problem is live and extinction-level; demographic data support that Parsi numbers have declined ca. 1.8% annually in India (from ca. 120,000 in 1951 to ca. 60,000 in 2020), with intermarriage cited as a primary factor. Reform Parsis and diaspora scholars attest the founding problem is partly obsolete: modern legal pluralism, secular education, and professional mobility have changed the assimilation conditions; the constraint persists as institutional inertia rather than as a response to extinction-level threat. External demographers (not affiliated with Parsi community authorities) document both the demographic decline and the causal ambiguity: causation by intermarriage is correlational, not mechanistically isolated from other factors (emigration, lower fertility, delayed marriage).
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   The extractiveness reading of 0.68 reflects the constraint's dual nature: genuine coordination function (small community cannot survive without endogamy enforcement) layered over asymmetric extraction (individuals lose agency over marriage choice, women lose inheritance equality, non-Parsis are categorically excluded). The constraint is NOT pure coordination (ε would be lower, suppression would be lower) because the extraction depends on active enforcement—priestly certification can be denied, individuals can be excommunicated, status can be revoked. Suppression is high (0.72) because alternatives (intermarriage, civil marriage without priestly blessing, reform readings) are actively suppressed through social sanction, inheritance loss, and ritual invalidity. Theater ratio is low-moderate (0.28): the priestly review function is genuine (ritual purity, doctrinal conformity), but enforcement activity increasingly serves to maintain endogamy exclusivity rather than to verify genuine ritual concerns. The measurement series shows extractiveness rising over the interval (0.51 to 0.68) while suppression requirement also rises: this pattern reflects how demographic pressure from assimilation drives the community to tighten enforcement, making the constraint more extractive over time. Theater ratio rises more slowly, indicating the functional core (ritual authority) persists but is increasingly oriented toward exclusion rather than genuine ritual practice.
 *
 * PERSPECTIVAL GAP:
 *   The priestly authority and community beneficiaries perceive this as genuine coordination—an essential mechanism for preserving a threatened minority. From their seat, the 'extraction' is the legitimate authority exercised to maintain community boundaries; the suppression is necessary enforcement of shared values. From the target seats (individuals, women, excluded partners), the same structure operates as enforced exclusion: choice is illusory ('marry within the faith or lose everything'), the priestly authority is experienced as gatekeeping and control, and the 'coordination' benefit is invisible because they are not permitted to participate in choosing the arrangement. The engine computes this divergence automatically from the structural data—the beneficiary/victim declarations, the directionality differences, and the power asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The priestly authority and community institution are the structural beneficiaries (d near 0.1): they collect authority, control membership, determine inheritance, and benefit from demographic preservation. Individuals intermarrying are full targets (d near 0.95): they face binary choice (forgo partner, forgo community) with identity-locked exit—the identity of 'Parsi Zoroastrian' is constituted through community membership, so exit is existential loss, not economic exit. Women occupy a complex position (d near 0.65): they benefit from community belonging and inheritance rights (if they marry within the faith according to law), but they pay through unequal ritual authority and property control. Their exit is also identity-locked (leaving the faith means abandoning family, cultural identity, and inherited status). Non-Parsi partners face categorical exclusion (no directionality mapping—they are not part of the arrangement but rather defined out of it). Reform Parsis occupy an observer seat with moderate power and analytical exit (they can leave the priesthood or the community, or maintain dual positions).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mild mandatrophy: the founding problem (demographic extinction of a minority community) was once live and urgent; demographic data confirm that Parsi population declined from 120,000 (1951) to 60,000 (2020), with intermarriage a documented factor. However, the causal chain is contested: reformers argue that modern legal pluralism, secular education, professional mobility, and diaspora conditions have fundamentally changed the extinction risk—Parsis can now preserve cultural identity without marriage-law enforcement, because cultural reproduction is decoupled from biological reproduction (adoption, cultural education, secular ceremonies). The constraint persists not primarily because the founding problem is alive, but because institutional authority (the priestly establishment and community council structures) benefits from maintaining marriage-law control. This is NOT complete mandatrophy—the founding problem has partial credibility—but it is the intermediate state where enforcement is increasingly defending the institution's authority rather than responding to the actual extinction risk. The rising extractiveness and theater ratio over the measurement interval reflect this drift: as demographic assimilation pressure shifts the community's composition (more diaspora, more secularized members), enforcement must intensify to maintain boundaries, and the functional core (ritual purity verification) becomes a smaller fraction of enforcement activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Parsi Zoroastrian reading of family law authority (priestly + endogamy + textual tradition) a live normative commitment, or is it a post-hoc rationalization of institutional preservation?',
    'Examine whether reform Parsi readings (rejecting endogamy but retaining priestly authority over ritual form, or rejecting priestly authority while retaining spiritual continuity) represent genuine alternatives within Zoroastrian tradition, or whether they amount to rejecting the tradition itself. Interview Parsi intellectuals, priestly establishment, and community members across diaspora/India to identify where disagreement is located: is it over interpretation of texts, or over whether texts should govern marriage at all?',
    'If reform readings represent live alternatives within tradition (not rejections), the constraint''s legitimacy rests on contested interpretation, not natural law—this shifts the engine''s framing from ''institutional preservation of an essential practice'' to ''contested authority over definition.'' If reform readings are external to the tradition, the constraint''s legitimacy is stronger within its own frame but its scope of application narrows (the commitment becomes binding only on traditional believers, not on all Parsis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the priestly authority reading reflects genuine textual commitment or institutional gatekeeping rationalization.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of interfaith marriage in Parsi communities primarily structural (external barriers: community sanction, inheritance law, priestly refusal to perform ritual) or internalized (individuals have adopted the belief that interfaith marriage is spiritually/culturally wrong and that Parsi identity requires priestly validation)?',
    'Post-exit trajectory: interview Parsis who have intermarried and left the community, and diaspora Parsis who have secularized marriage but retained community ties. Do they report that the suppression persists after the structural barriers are removed (internalized guilt, self-policing, identity fragmentation)? Or does suppression collapse once the external gatekeeping is bypassed? Comparative data from diaspora communities with different enforcement histories (Iran, Mumbai, North America) would show whether internalization is uniform or locally contingent.',
    'If suppression is primarily structural, reforming the constraint (opening priestly authority to interfaith weddings, or recognizing secular marriage as valid) would likely succeed. If suppression is substantially internalized, reform alone would not resolve the constraint—individuals would continue to experience exit as identity loss even without external enforcement. The two mechanisms produce different remediation strategies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural (external enforcement) or internalized (belief system) or mixed.').

omega_variable(
    demographic_causation_ambiguity,
    'Is Parsi population decline causally driven by intermarriage, or is intermarriage a symptom/correlate of assimilation driven by other factors (diaspora conditions, education, economic mobility, delayed marriage, lower overall fertility)?',
    'Comparative demographic analysis: examine Parsi communities in different institutional contexts (traditional communities in Mumbai with strong priestly authority vs. diaspora communities with weak enforcement) and control for non-marriage assimilation factors (professional mobility, language shift, education attainment, migration distance from homeland). If decline rates are similar across high-enforcement and low-enforcement communities, intermarriage is correlational, not causal.',
    'If intermarriage is causal, the constraint''s founding problem (extinction) remains live and the enforcement logic is justified. If intermarriage is a symptom rather than a cause, the constraint addresses a surface manifestation rather than the underlying threat—the community would face assimilation regardless of marriage-law enforcement, and the constraint''s persistence is institutional inertia rather than response to genuine extinction risk (shifts the classification toward piton/theater territory).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_causation_ambiguity, empirical, 'Whether population decline is caused by intermarriage or driven by other assimilation mechanisms.').

omega_variable(
    identity_lock_mechanism_parsi,
    'What specific identity-fusion mechanisms bind individuals to the Parsi community despite marriage-law constraints? Is it professional identity (career paths within Parsi business/professional networks), relational identity (familial ties, spousal/parental bonds), ideological identity (belief in Zoroastrian spiritual tradition), or institutional identity (the community has become constitutive of their self-concept)?',
    'Interview Parsi individuals across diaspora and traditional communities, probing what would it would take to exit the community. For individuals who have intermarried or secularized: which exit barriers proved strongest? For individuals adhering to tradition: which elements would they preserve even if priestly authority relaxed? This illuminates whether identity-lock is primarily through one mechanism or is multi-layered.',
    'Different identity-lock mechanisms enable different reform strategies: if the lock is primarily professional (kinship business networks), economic reform (opening professional networks to interfaith Parsis) would loosen it. If the lock is relational, only familial acceptance of interfaith marriage would loosen it. If the lock is ideological, only spiritual reform (reframing Zoroastrianism as compatible with interfaith marriage) would loosen it. Understanding the mechanism targets reform efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_parsi, empirical, 'Which identity-fusion mechanisms (professional, relational, ideological, institutional) bind individuals to community despite constraint.').

omega_variable(
    priestly_authority_legitimacy_grounds,
    'From what sources does the priestly authority derive its legitimacy to determine marriage validity? Is legitimacy grounded in textual tradition (Khordeh Avesta, continuous transmission from Sassanid priesthood), in community consent (the community grants priests authority because they want boundaries enforced), in expertise (priests are trained in Zoroastrian law and ritual), or in institutional inertia (priests have always decided these matters)?',
    'Analyze historical sources on Parsi priesthood establishment and authority legitimation. Examine how priestly authority is justified in contemporary Parsi institutions (community councils, fire temples, educational materials). Interview priests on why they claim authority over marriage; interview laypeople on why they recognize priestly authority. Assess whether the grounds have shifted over time (from text-based to institutional to consent-based).',
    'Different legitimacy grounds are vulnerable to different challenges: if authority rests primarily on textual tradition, reform readings of texts can challenge it. If authority rests on community consent, organizational reform (admitting lay voices to authority structures) can challenge it. If authority rests on expertise, opening expertise recognition to trained reformers can challenge it. If authority rests on pure institutional inertia, the constraint becomes a piton candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_authority_legitimacy_grounds, conceptual, 'What grounds the priestly authority''s legitimacy to determine marriage validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(fami_be_t5, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fami_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fami_be_t25, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fami_su_t5, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(fami_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fami_su_t25, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the 'family_law_authority' kernel family. Five constraints are authored from this contested kernel, each representing a different reading (Parsi Zoroastrian, Christian Canonical, Muslim Shariat, Hindu Dharmashastra, Secular Contractual). Each constraint has its own ε (extractiveness), its own beneficiary/victim structure, its own claimed type, and its own temporal trajectory. They are NOT variants of one constraint; they are structurally distinct constraints grounded in different authority sources and serving different communities. The family is linked bidirectionally via network.affects_constraints: changes in one reading (e.g., secular state codification of marriage law) create structural pressure on all other readings. All five constraints share the same kernel (the standing arrangement that determines marriage legitimacy) but diverge in how they answer 'who decides and on what grounds.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
