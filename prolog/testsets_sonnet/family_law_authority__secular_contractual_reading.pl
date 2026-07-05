% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Contractual Reading of Marriage Authority (Special Marriage Act model)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint models the secular civil-contract reading of marriage
 *   authority: a legal architecture in which state registration, not
 *   religious sanction, is the sole criterion of marital validity, rights
 *   attach symmetrically by sex, and interfaith union requires no conversion.
 *   This is one of five readings of a shared kernel — 'what makes a marriage
 *   valid and whose law governs it' — held simultaneously by different
 *   institutional actors within the same polity. The secular reading does not
 *   replace the others; it coexists as an opt-in alternative track that pulls
 *   jurisdiction away from religious authorities only for the couples who
 *   choose it. Extraction is low because the mechanism's coordination
 *   function (portable, religion-neutral legal status; gender-symmetric
 *   defaults) dominates its cost, but it is not zero: religious authorities
 *   lose real jurisdiction and standing, and couples who choose the track pay
 *   a real, uncompensated social cost the statute does not internalize.
 *
 * KEY AGENTS:
 *   - civil_registration_authorities: administers the sole validity criterion (institutional/analytical)
 *   - interfaith_couples: primary beneficiary of an otherwise unavailable marriage path (moderate/mobile)
 *   - women_seeking_gender_symmetric_exit_rights: beneficiary of symmetric defaults, payer of social cost (moderate/constrained)
 *   - religious_authorities_losing_jurisdiction: payer of jurisdictional loss (organized/constrained)
 *   - couples_facing_social_sanction_for_civil_registration: payer of the mechanism's uninternalized publicity cost (powerless/constrained)
 *   - the_state: beneficiary of a uniform administrative category (institutional/analytical)
 *   - personal_law_boards: excluded voice, contests scope outside this mechanism (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.28).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Contractual Reading of Marriage Authority (Special Marriage Act model)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, 'ce4f5226-b3f8-42b8-93d6-4228fc232c12').
narrative_ontology:cs_kernel_codification('ce4f5226-b3f8-42b8-93d6-4228fc232c12', formalized).
narrative_ontology:cs_authority_grounding('ce4f5226-b3f8-42b8-93d6-4228fc232c12', expertise).
narrative_ontology:cs_interpretation_layer_present('ce4f5226-b3f8-42b8-93d6-4228fc232c12').
narrative_ontology:cs_reading_relation('ce4f5226-b3f8-42b8-93d6-4228fc232c12', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce4f5226-b3f8-42b8-93d6-4228fc232c12', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce4f5226-b3f8-42b8-93d6-4228fc232c12', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce4f5226-b3f8-42b8-93d6-4228fc232c12', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('ce4f5226-b3f8-42b8-93d6-4228fc232c12', foundational, state_registration_sole_validity_criterion).
narrative_ontology:cs_axiom_status(state_registration_sole_validity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ce4f5226-b3f8-42b8-93d6-4228fc232c12', state_registration_sole_validity_criterion, conventional).
narrative_ontology:cs_axiom('ce4f5226-b3f8-42b8-93d6-4228fc232c12', foundational, marital_rights_gender_symmetric_by_default).
narrative_ontology:cs_axiom_status(marital_rights_gender_symmetric_by_default, holdable).
narrative_ontology:cs_axiom_grounding('ce4f5226-b3f8-42b8-93d6-4228fc232c12', marital_rights_gender_symmetric_by_default, deontological).
narrative_ontology:cs_reference_frame('ce4f5226-b3f8-42b8-93d6-4228fc232c12', colonial_era_religion_exclusive_personal_law).
narrative_ontology:cs_drift_state('ce4f5226-b3f8-42b8-93d6-4228fc232c12', contemporary_multi_track_family_law_regime, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce4f5226-b3f8-42b8-93d6-4228fc232c12', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, civil_registration_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_exit_rights).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_authorities_losing_jurisdiction).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, couples_facing_social_sanction_for_civil_registration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, the_state).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_exit_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the state marriage register, sets notice periods, verifies age and consent, and issues the certificate that is the sole legal proof of marriage under this reading. Does not adjudicate religious validity at all — registration is both necessary and sufficient. Benefits from having a single, auditable civil status record independent of religious community.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registration_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Could not marry within any single religious personal-law system without one partner converting or being excluded. The civil registration path lets them marry without either conversion or religious sanction, at the cost of a mandatory public notice period that exposes the marriage to family and community objection before it is finalized.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Under this reading, divorce, maintenance, and succession rights attach symmetrically regardless of sex, unlike several personal-law regimes with asymmetric grounds or timelines for men and women. They pay a cost in social standing where community norms treat civil marriage or civil divorce as a defection from religious community, and in practice still face uneven bargaining power in negotiated settlements even though the statute is symmetric.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_exit_rights, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, women_seeking_gender_symmetric_exit_rights, payer).

% Lose the exclusive power to define who may marry whom, on what terms, and to adjudicate the marriage's validity, whenever couples opt into the civil track instead of the personal-law track. Their loss is jurisdictional and reputational rather than directly financial — the civil register is a visible alternative forum that couples can choose instead of them.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities_losing_jurisdiction, payer,
    organized, generational, constrained, national).

% Gain the legal right to marry civilly but bear the informal cost the statute cannot touch: family ostracism, community boycott, or in extreme cases violence, precisely because the civil track's publicity (the mandatory notice period, posted publicly) makes the choice visible before the couple has any protective status.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, couples_facing_social_sanction_for_civil_registration, payer,
    powerless, biographical, constrained, regional).

% Gains a uniform civil-status category usable for taxation, inheritance default rules, immigration sponsorship, and census purposes, independent of the citizen's religion. This uniformity is itself a governance asset: it lets the state treat marriage as a legible administrative fact rather than a religiously variable one.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, the_state, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, the_state, agenda_setter).

% Would object that the civil track's gender-symmetric defaults override community self-governance and doctrinal marriage rules they consider theologically mandatory, but they have no formal voice inside the civil registration process itself — their objections surface only in parallel political and judicial contests over the scope of religious personal law, not within this constraint's own machinery.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, personal_law_boards, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, religion-neutral legal mechanism by which any two adult citizens can obtain a marriage with uniform, predictable, and gender-symmetric legal consequences — solving the coordination problem of interfaith union and the coordination problem of a citizen needing one portable legal status recognized uniformly across jurisdictions and institutions (banks, hospitals, immigration, courts) regardless of religious affiliation.
% TRANSFER_FUNCTION: Moves adjudicatory authority over marital validity, and control over inheritance/divorce/maintenance defaults, away from religious community bodies and toward the state; within the couple, it moves formal legal leverage toward gender-symmetric defaults, redistributing bargaining power at exit relative to several personal-law regimes.
% ABSENT_VOICES: Personal law boards and religious community elders who consider marriage's proper governance to be doctrinal, not civil, have no seat inside the registration process; they contest the arrangement's scope in legislatures and courts rather than within it. Extended family networks who bear reputational stakes in endogamous marriage are similarly outside the transaction the statute recognizes.
% DISAPPEARANCE_RATIONALE: If the civil registration track vanished, interfaith and inter-caste couples would lose their only marriage path not requiring conversion or community consent; gender-symmetric divorce and succession defaults would disappear for anyone currently relying on them; and the state would lose its uniform administrative category for marital status, reverting fully to religion-differentiated personal law for every citizen.
% FOUNDING_PROBLEM: Colonial and early postcolonial legal systems left marriage governed entirely by religious personal law, which structurally barred interfaith marriage without conversion, applied gender-asymmetric rights within several traditions, and left the state without any religion-neutral category for marital status administration.
% FOUNDING_PROBLEM_CORROBORATION: Feminist legal scholars and comparative family-law researchers outside any religious authority structure attest the founding problem (interfaith exclusion, gender asymmetry) remains substantially live and that uptake of the civil track, while growing, is still suppressed by the public-notice requirement's exposure of couples to social sanction — a defect the state itself has acknowledged in law-reform commission reports recommending removal of the notice-publication requirement, which is independent corroboration that the mechanism's own administrators see the founding problem as only partially solved.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) and mostly reflects jurisdictional transfer away from religious bodies rather than rent extraction from couples — the couples who opt in are net beneficiaries of the mechanism itself. Suppression is moderate-low and has been declining across the interval (0.30 -> 0.22) as social acceptance of civil marriage has grown and mandatory-notice publication requirements have been partially relaxed in some jurisdictions; the mechanism required more active insulation from religious-authority contestation in its early decades than it does now. Theater ratio is low and slowly rising (0.10 -> 0.15) — a modest amount of the registration process (public notice display, waiting period) now functions more as inherited ritual than as functionally necessary verification, but this is not the dominant feature.
 *
 * PERSPECTIVAL GAP:
 *   From the civil registration authority's seat, this looks like straightforward administrative coordination — a clean, low-friction alternative track. From a religious authority's seat, the identical mechanism looks like an ongoing erosion of jurisdiction with no compensating benefit; that seat's constrained exit (it cannot exit the state's civil registration system, only lose relevance within it) drives a materially different computed classification even though the underlying structural facts are the same story.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil registration authorities and the state sit at the low end of directionality — they are structural beneficiaries who administer the mechanism and gain a governance asset (legible civil status) from its operation. Interfaith couples and women seeking symmetric exit rights are declared beneficiaries because the mechanism exists specifically to serve them, though the second group also carries a genuine cost (social sanction, uneven real bargaining power despite formal symmetry) that keeps their directionality from sitting fully at the beneficiary pole — hence the secondary payer role. Religious authorities and personal-law boards are targets: the mechanism's entire function, from their seat, is the erosion of their exclusive jurisdiction, which is why they carry high directionality despite considerable organized power — power does not buy exit from a jurisdiction-stripping mechanism once a couple opts in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interfaith exclusion, gender-asymmetric personal law, absence of a religion-neutral civil status) remains substantially live rather than resolved, which is why founding_problem_status is 'contested' rather than 'dead': the mechanism has not outlived its function, but its function is only partially realized because the publicity cost it imposes (public notice) suppresses uptake among exactly the couples most likely to face social sanction, meaning the mechanism under-serves its own founding purpose. This is not mandatrophy in the classic sense (mandate outlived, husk persists) — it is closer to an unfinished mandate: the coordination function is real and current, but its own design (public notice) partially defeats it. Classifying this as rope rather than snare or tangled_rope prevents mislabeling a genuinely under-extractive coordination mechanism as extraction merely because it has real, uncompensated costs for some beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opt_in_versus_default_erosion,
    'Does the availability of an opt-in secular track structurally erode the personal-law readings even for couples who never use it, by changing the bargaining baseline (exit threat value) within religious-law marriages?',
    'Compare divorce/maintenance settlement outcomes within personal-law-governed marriages before and after the secular track''s availability and enforcement expanded, controlling for other legal reforms.',
    'If the mere existence of the secular track raises the effective floor of rights within personal-law marriages (an outside option effect), this reading''s influence on siblings should be characterized as ''influences'' rather than pure ''coexists_with'' in effect even though no single framework is logically foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opt_in_versus_default_erosion, empirical, 'Whether opt-in availability has a spillover bargaining effect on non-opting couples.').

omega_variable(
    notice_requirement_purpose_versus_effect,
    'Is the mandatory public notice period a genuine verification safeguard (preventing bigamy, coercion, fraud) or has it become primarily a mechanism that exposes vulnerable couples to social sanction, with verification as its residual rather than primary function?',
    'Compare bigamy/coercion detection rates attributable to the notice period against documented instances of family intervention, harassment, or violence triggered by notice publication, using court and NGO case records.',
    'If the notice requirement''s actual operative function is exposure rather than verification, the theater_ratio trajectory understates the degree to which a nominally protective procedural step has become extractive of the couples it claims to protect, which would push this reading closer to tangled_rope for the specific sub-population facing sanction risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_requirement_purpose_versus_effect, empirical, 'Whether the notice period still serves its stated verification function or has drifted toward exposure risk.').

omega_variable(
    civil_marriage_naturalness_versus_construction,
    'Is state-registration-as-sole-validity-criterion a neutral administrative fact, or is treating civil registration as sufficient itself a substantive normative claim (that the state, not religious community, is the proper final arbiter of marital status) dressed as procedural neutrality?',
    'Examine legislative debate records and constitutional court reasoning on whether civil marriage''s exclusivity as legal proof was framed as neutral administration or as an affirmative displacement of religious authority.',
    'If the ''neutrality'' framing itself constitutes a normative displacement of religious jurisdiction, the state''s beneficiary status is less incidental than administrative convenience suggests — it would sharpen the tangled_rope-adjacent reading of the relationship between this constraint and the personal_law_boards'' loss.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_marriage_naturalness_versus_construction, conceptual, 'Whether procedural neutrality claims mask a substantive authority displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1954, family_law_authority__secular_contractual_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(fami_tr_t1970, family_law_authority__secular_contractual_reading, theater_ratio, 1970, 0.11).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__secular_contractual_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(fami_tr_t2006, family_law_authority__secular_contractual_reading, theater_ratio, 2006, 0.13).
narrative_ontology:measurement(fami_tr_t2018, family_law_authority__secular_contractual_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__secular_contractual_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fami_be_t1954, family_law_authority__secular_contractual_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(fami_be_t1970, family_law_authority__secular_contractual_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__secular_contractual_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(fami_be_t2006, family_law_authority__secular_contractual_reading, base_extractiveness, 2006, 0.25).
narrative_ontology:measurement(fami_be_t2018, family_law_authority__secular_contractual_reading, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__secular_contractual_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1954, family_law_authority__secular_contractual_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(fami_su_t1970, family_law_authority__secular_contractual_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__secular_contractual_reading, suppression_requirement, 1990, 0.26).
narrative_ontology:measurement(fami_su_t2006, family_law_authority__secular_contractual_reading, suppression_requirement, 2006, 0.24).
narrative_ontology:measurement(fami_su_t2018, family_law_authority__secular_contractual_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__secular_contractual_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories forming the family_law_authority kernel family. Each sibling reading (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading) instantiates a structurally distinct claim about what makes a marriage valid and who has jurisdiction over it, with different beneficiary/victim sets and different extraction profiles. This reading has the lowest extractiveness in the family because it is opt-in and does not claim exclusive jurisdiction over any citizen who does not choose it — its downstream structural pressure on siblings runs through the bargaining-baseline and legitimacy-erosion channels documented in the omegas above, not through direct displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
