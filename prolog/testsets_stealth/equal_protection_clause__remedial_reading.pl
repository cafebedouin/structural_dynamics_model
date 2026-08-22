% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [CONTESTED]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial Reading (Race-Conscious Subordination Redress Mandate)
 *   domain: constitutional law / political philosophy / education policy
 *
 * SUMMARY:
 *   The equal protection clause is a single contested kernel — one sentence
 *   of constitutional text — read three ways. This file instantiates the
 *   remedial reading: equal protection requires race-conscious remediation of
 *   historical group subordination, with a declared sunset at remediation's
 *   completion. Under this reading, historically subordinated racial groups
 *   are the beneficiaries of a constitutionally mandated transfer of
 *   opportunities, and individual members of non-preferred groups bear its
 *   costs. The epsilon referent is the standing arrangement under contest —
 *   the remedial mandate as actually instantiated (admissions preferences,
 *   contracting set-asides, employment programs) — assessed by this reading's
 *   own lights: the reading acknowledges the burden on non-preferred
 *   individuals (its own narrow-tailoring doctrine exists to minimize it)
 *   while holding the burden justified. The sibling readings —
 *   colorblind_reading and diversity_reading — are separate constraint files
 *   with different victim sets and different epsilon values; per the
 *   one-reading discipline, the kernel contest is not adjudicated here. The
 *   interval runs from Bakke (1978), where the remedial rationale first
 *   survived as doctrine, to SFFA (2023), where the Court repudiated it in
 *   education.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: primary beneficiary (organized/generational) — receives the opportunities the mandate channels
 *   - non_preferred_racial_group_members: primary target (moderate/biographical, trapped) — bears the classification's total, unchosen per-instance burden
 *   - federal_courts: agenda-setter (institutional) — administers the doctrinal ratchet and ultimately struck the mandate's core application
 *   - remedial_program_administrators: implementing agenda-setter (institutional, constrained) — bears compliance and dismantling costs
 *   - civil_rights_advocacy_organizations: identity-locked beneficiary (organized) — organizational rents fused with the mandate's persistence
 *   - colorblind_constitutional_advocates: excluded voice (organized, mobile) — holds the sibling reading that ultimately prevailed
 *   - constitutional_law_academy: analytical observer — sees the full kernel structure no participating seat can see from inside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.66).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.55).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause — Remedial Reading (Race-Conscious Subordination Redress Mandate)").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional law / political philosophy / education policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'e1de8691-6198-412c-9722-8faa3b867c80').
narrative_ontology:cs_kernel_codification('e1de8691-6198-412c-9722-8faa3b867c80', fixed_text).
narrative_ontology:cs_authority_grounding('e1de8691-6198-412c-9722-8faa3b867c80', lineage).
narrative_ontology:cs_interpretation_layer_present('e1de8691-6198-412c-9722-8faa3b867c80').
narrative_ontology:cs_reading_relation('e1de8691-6198-412c-9722-8faa3b867c80', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('e1de8691-6198-412c-9722-8faa3b867c80', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('e1de8691-6198-412c-9722-8faa3b867c80', foundational, substantive_equality_requires_group_redress).
narrative_ontology:cs_axiom_status(substantive_equality_requires_group_redress, holdable).
narrative_ontology:cs_axiom_grounding('e1de8691-6198-412c-9722-8faa3b867c80', substantive_equality_requires_group_redress, deontological).
narrative_ontology:cs_axiom('e1de8691-6198-412c-9722-8faa3b867c80', foundational, race_neutral_means_insufficient_for_redress).
narrative_ontology:cs_axiom_status(race_neutral_means_insufficient_for_redress, holdable).
narrative_ontology:cs_axiom_grounding('e1de8691-6198-412c-9722-8faa3b867c80', race_neutral_means_insufficient_for_redress, instrumental).
narrative_ontology:cs_reference_frame('e1de8691-6198-412c-9722-8faa3b867c80', antisubordination_redress_charter).
narrative_ontology:cs_drift_state('e1de8691-6198-412c-9722-8faa3b867c80', post_sffa_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e1de8691-6198-412c-9722-8faa3b867c80', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_racial_group_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, remedial_program_administrators).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The groups the mandate exists to advance, whose members receive preferential access to admissions seats, public contracts, and employment positions the mandate channels toward them. Their position in the structure is defined by a history they did not choose; no member can unilaterally exit the beneficiary role, because exit would mean declaring the group's subordination remedied, which is precisely what the mandate exists to determine.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Individual applicants, bidders, and employees disfavored by the classification: denied a seat, a contract, or a position because of their race. The burden is total per instance — the denied opportunity does not partially accrue to them — and unchosen; no action available to any member removes them from the classification's reach. Their collective response runs through litigation campaigns and ballot initiatives, which operate on decade timescales and require resources most members do not hold individually.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_racial_group_members, payer,
    moderate, biographical, trapped, national).

% Author and administer the doctrine: which remedial justifications survive, what narrow tailoring requires, which programs fall. They built the ratchet (Croson, Adarand), partially relaxed it (Grutter), and ultimately struck the mandate's core application (SFFA). They are bound by precedent and by a composition they do not control; their movement is doctrinal evolution, not withdrawal.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Admissions offices, procurement agencies, and school districts that operate race-conscious programs day to day. They bear compliance costs, record-keeping burdens, and litigation exposure while implementing the mandate; noncompliance is unavailable to them while the mandate stands, so their discretion is confined to how, not whether. When the doctrine tightens, they absorb the redesign costs; when it collapses, they absorb the dismantling costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, remedial_program_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, remedial_program_administrators, payer).

% Organizations whose institutional purpose, standing, and funding are fused with the remediation project. They litigate to preserve and extend the mandate and collect organizational rents from its persistence: mission justification, member mobilization, grant funding. If the mandate were completed or struck, their central constitutional instrument and much of their organizational identity would collapse with it. Exit is not available to them without dissolving the mission they are.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% The movement holding that equal protection forbids all governmental racial classifications. Within this reading's framework their objection is not an internal constitutional claim but a rejection of the reading's premise — the unit of concern itself — so they are structurally outside the conversation the mandate constitutes. They are not without recourse: their position is fully articulated in a sibling reading of the same clause, and they built the four-decade litigation campaign that eventually won it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_constitutional_advocates, excluded,
    organized, biographical, mobile, national).

% Scholars who map the clause's competing readings, trace the doctrine's drift across the case law, and supply the arguments each side litigates with. They collect nothing and pay nothing under the mandate; their seat is analytical, seeing the full kernel structure that no participating seat can see from inside.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide redress of group-level subordination that no individual actor and no race-neutral rule performs unilaterally: it solves the collective-action problem that formally equal rules freeze accumulated group disadvantage in place, by directing institutions to allocate opportunities by race until group disparities close.
% TRANSFER_FUNCTION: Moves concrete opportunities — admissions seats, public contracts, employment positions, political representation — from individual members of non-preferred racial groups to members of historically subordinated groups, and moves compliance, record-keeping, and litigation costs onto administering institutions, for as long as the remediation project runs.
% ABSENT_VOICES: The individual non-preferred applicants whose denials constitute the mandate's operation are present only as aggregate categories, not as voices; and colorblind constitutional advocates are excluded by the reading's own premise — their objection denies the reading's unit of concern (the group), so within this framework it registers as external rejection rather than internal dissent. Both voices live in the sibling readings of the same clause rather than inside this one.
% DISAPPEARANCE_RATIONALE: Admissions, public procurement, and civil-rights enforcement are organized around the mandate. If it vanished overnight, institutions would revert to formally race-neutral rules, advocacy organizations would lose their central constitutional instrument and much of their reason for existence, and — on this reading's own lights — accumulated group disparities would persist unremedied, since no other mechanism in the standing arrangement performs the redress function.
% FOUNDING_PROBLEM: Slavery, segregation, and formal exclusion produced durable group-level disparities that facially equal treatment does not undo; the founding problem is whether and how a constitutional order committed to equality must act race-consciously to redress subordination it previously enforced.
% FOUNDING_PROBLEM_CORROBORATION: Persistent measured disparities — in wealth, incarceration, and educational attainment — are documented by federal statistical agencies (Census Bureau, Bureau of Justice Statistics) with no stake in the mandate's persistence; the Croson Court itself acknowledged the reality of past discrimination while contesting the remedy; and the Fullilove congressional findings (1980) attested the problem's scope from outside the beneficiary set. No corroborating source outside the benefiting parties attests that the problem is solved.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type: scaffold. The reading's own justification is transitional — the mandate exists to carry institutions from entrenched group disparity to substantive equality and then dissolve, and its temporariness is declared in the doctrine itself (temporary measures, time-limited expectations). requires_active_enforcement is true: the mandate holds only through court orders, compliance machinery, and funding conditions. Metrics are authored independently of the claim. Extractiveness 0.66 is high because the burden on each non-preferred individual is total and unchosen — the denied seat does not partially accrue to them. Suppression 0.55 is the structural lack of alternatives while the mandate stands: classified individuals cannot exit the classification, administering institutions cannot decline compliance; suppression is authored as a raw structural property, unscaled — the engine's directionality and scope scaling apply only to extractiveness. Theater_ratio 0.58 records the end-state drift in which remedial substance was displaced by diversity rhetoric and race-neutral proxy compliance. Accessibility_collapse 0.45 reflects that workable alternatives (class-based, place-based, race-neutral proxies) persist even though this reading holds them inadequate to group redress. Resistance 0.75 records the sustained, organized, ultimately successful opposition — ballot initiatives and four decades of litigation. The measurement series run on one shared grid (7 points x 3 metrics): extractiveness is roughly flat (intrinsic to the mandate, modulated by tailoring doctrine), theater rises monotonically (proxy-compliance drift crossing 0.5 by 2014), and suppression_requirement traces the enforcement ratchet (0.35 to 0.72 through Croson and Adarand) and its collapse (0.30 at SFFA). The suppression scalar (0.55) and the series final (0.30) diverge deliberately: the scalar is the constraint's structural coercion while operative; the series is enforcement-machinery intensity, whose collapse at 2023 is the story's terminus, not its summary. All values are observed historical record.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same doctrine. From the beneficiary seat (subordinated groups; identity-locked advocacy organizations), the arrangement is a deliberately temporary instrument of justice whose persistence is its success condition. From the payer seat (non-preferred individuals, trapped), the same structure operates as extraction with no exit and no consent — a burden imposed by a history they did not author, administered by institutions they do not control, with the denied opportunity never partially returned. From the courts' seat, it is neither: a doctrine to be ratcheted, narrowed, and ultimately withdrawn. The advocacy organizations' identity lock matters structurally: their exit is unavailable without dissolving the mission they are, so their seat reads the mandate as permanent even while the doctrine declares it temporary — a structural pressure toward exactly the proxy-compliance drift the theater series records.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: historically_subordinated_racial_groups (d near the beneficiary end — the mandate subsidizes their members' access) and derivatively civil_rights_advocacy_organizations (d low but nonzero — they collect organizational rents: standing, mission, funding — rather than the opportunities themselves). Victims: non_preferred_racial_group_members (d near the full-target end — they bear the transfer, trapped, no arbitrage available). Federal courts and program administrators are agenda-setters, not collectors: they neither receive the transfer nor bear its opportunity cost, though compliance costs pull them slightly toward the target end. Scope is national, which amplifies effective extraction modestly through verification difficulty — a mandate spanning thousands of institutions cannot be narrowly audited, so declared compliance substitutes for verified compliance, feeding the theater drift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — group subordination and its durable disparities — remains live on every corroborating measure, so the mandate did not atrophy in the classic sense: founding_problem_status=live crossed with disappearance_verdict=world_rearranges yields no zombie flag. But the constraint's death was not its sunset firing. The declared completion condition never acquired a metric (see omega remediation_completeness_metric), and the theater series shows the mandate drifting toward proxy compliance — remedial substance displacing into diversity rhetoric — for two decades before SFFA struck it. The constraint was killed by external repudiation, not completion: the scaffold's own exit condition was never satisfiable, which is the structural reason a remedial transitional constraint tends either to complete and dissolve or to drift and persist theatrically rather than switch itself off on its own terms. The mandatrophy question this story answers is therefore not 'did the mandate outlive its function' but 'could it ever have self-terminated' — and the structural answer is no, which is the finding the corpus should carry forward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (remedial_reading) of the equal protection kernel; which structural element locates the disagreement with the sibling readings (colorblind_reading, diversity_reading)?',
    'Locate the contested element across the sibling files: the unit of constitutional concern — formal individual equality (colorblind) versus group substantive equality (remedial) versus institutional benefit to all (diversity). Each sibling instantiates its own constraint; cross-file comparison of victim and beneficiary sets locates the disagreement precisely.',
    'Adopting the colorblind sibling empties this story''s beneficiary set and converts its victims into vindicated rights-bearers; adopting the diversity sibling replaces the remedial mandate with a benefit-all framing and lowers epsilon. This file''s classification holds only within the remedial reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the equal protection kernel; disagreement located in the unit of constitutional concern.').

omega_variable(
    remediation_completeness_metric,
    'The declared sunset condition — remediation complete — has no agreed metric: parity in which outcomes (wealth, representation, incarceration, attainment), measured against what baseline, over what horizon?',
    'No resolution mechanism exists: every proposed parity metric embeds a contested conception of equality, and the parties dispute the metrics themselves. The sunset is indeterminate by construction.',
    'An indeterminate sunset means the constraint cannot self-terminate; had the mandate not been struck externally (SFFA), it would have drifted toward permanent operation — transitional form decaying into steady-state hybrid or theater-sustained remnant. The theater_ratio trajectory (0.20 to 0.58) records exactly this drift beginning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_completeness_metric, conceptual, 'The declared sunset lacks an operational completion metric — the classic transitional-constraint failure mode.').

omega_variable(
    beneficiary_boundary_contestation,
    'Who counts as historically subordinated for remediation — where does the beneficiary set''s boundary sit (which groups, which histories, whether newly subordinated groups qualify, how intra-group heterogeneity is handled)?',
    'No principled criterion has stabilized; the boundary is set case-by-case through political and litigation contestation. Track agency and lower-court classifications of eligible groups over time.',
    'Boundary drift changes both beneficiary and victim sets and therefore the directionality structure the engine derives; a widening boundary dilutes per-beneficiary concentration, a narrowing one concentrates it and sharpens the payer seat''s burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_boundary_contestation, conceptual, 'The beneficiary set''s boundary is politically constituted and unstable.').

omega_variable(
    extraction_vs_tailoring_artifact,
    'Is the measured per-instance extraction intrinsic to the race-conscious mandate, or an artifact of narrow-tailoring doctrine forbidding finer-grained remedies (blunt classifications where calibrated, individualized remedies are held unconstitutional)?',
    'Compare burden profiles under regimes with different tailoring latitude: programs with individualized review versus rigid set-asides; state regimes before and after initiative bans (Prop 209).',
    'If artifact, epsilon is doctrine-contingent rather than mandate-intrinsic, and a differently tailored remedial reading would compute lower extraction; if intrinsic, any race-conscious mandate carries this burden profile and the reading''s own doctrine cannot reduce it below the floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_tailoring_artifact, empirical, 'Whether extraction is intrinsic to race-conscious mandates or produced by the tailoring doctrine''s constraints.').

omega_variable(
    post_sffa_residual_scope,
    'Does the remedial reading retain operative force in domains the 2023 repudiation did not reach (public contracting, voting rights, employment programs), or is it extinguished wholesale?',
    'Lower-court litigation trajectory post-2023 and agency program terminations; track surviving race-conscious programs by domain.',
    'If residual pockets survive, the constraint is partially repudiated and its theater ratio should keep rising in the remnants; if extinguished wholesale, the reading survives only as normative scholarship and this constraint is fully historical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_sffa_residual_scope, empirical, 'Scope of the mandate''s survival after the 2023 repudiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1985, equal_protection_clause__remedial_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(equa_tr_t1985, observed).
narrative_ontology:measurement(equa_tr_t1992, equal_protection_clause__remedial_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement_basis(equa_tr_t1992, observed).
narrative_ontology:measurement(equa_tr_t1999, equal_protection_clause__remedial_reading, theater_ratio, 1999, 0.34).
narrative_ontology:measurement_basis(equa_tr_t1999, observed).
narrative_ontology:measurement(equa_tr_t2006, equal_protection_clause__remedial_reading, theater_ratio, 2006, 0.42).
narrative_ontology:measurement_basis(equa_tr_t2006, observed).
narrative_ontology:measurement(equa_tr_t2014, equal_protection_clause__remedial_reading, theater_ratio, 2014, 0.5).
narrative_ontology:measurement_basis(equa_tr_t2014, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.58).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.62).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1985, equal_protection_clause__remedial_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement_basis(equa_be_t1985, observed).
narrative_ontology:measurement(equa_be_t1992, equal_protection_clause__remedial_reading, base_extractiveness, 1992, 0.7).
narrative_ontology:measurement_basis(equa_be_t1992, observed).
narrative_ontology:measurement(equa_be_t1999, equal_protection_clause__remedial_reading, base_extractiveness, 1999, 0.67).
narrative_ontology:measurement_basis(equa_be_t1999, observed).
narrative_ontology:measurement(equa_be_t2006, equal_protection_clause__remedial_reading, base_extractiveness, 2006, 0.66).
narrative_ontology:measurement_basis(equa_be_t2006, observed).
narrative_ontology:measurement(equa_be_t2014, equal_protection_clause__remedial_reading, base_extractiveness, 2014, 0.65).
narrative_ontology:measurement_basis(equa_be_t2014, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement_basis(equa_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1985, equal_protection_clause__remedial_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement_basis(equa_su_t1985, observed).
narrative_ontology:measurement(equa_su_t1992, equal_protection_clause__remedial_reading, suppression_requirement, 1992, 0.6).
narrative_ontology:measurement_basis(equa_su_t1992, observed).
narrative_ontology:measurement(equa_su_t1999, equal_protection_clause__remedial_reading, suppression_requirement, 1999, 0.72).
narrative_ontology:measurement_basis(equa_su_t1999, observed).
narrative_ontology:measurement(equa_su_t2006, equal_protection_clause__remedial_reading, suppression_requirement, 2006, 0.66).
narrative_ontology:measurement_basis(equa_su_t2006, observed).
narrative_ontology:measurement(equa_su_t2014, equal_protection_clause__remedial_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement_basis(equa_su_t2014, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.3).
narrative_ontology:measurement_basis(equa_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal protection clause is one kernel — a single sentence of constitutional text — decomposed into three constraint stories under the ε-invariance principle. This file instantiates the remedial reading (beneficiaries: historically subordinated groups; victims: individual non-preferred members; ε 0.66; transitional with declared sunset). The sibling files instantiate the colorblind reading (no beneficiaries; all subjects of state racial classification as victims) and the diversity reading (beneficiaries: all students; victims: denied non-preferred applicants; lower ε). The ε values differ because the victim and beneficiary sets differ — the readings are different constraints, not one constraint under different observables. Historical dependency: the remedial reading was the operative reading from Bakke (1978); its doctrinal constriction (Croson 1989, Adarand 1995) created the structural conditions under which institutions and the Court migrated to the diversity justification (Grutter 2003), and both were repudiated together in SFFA (2023).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
