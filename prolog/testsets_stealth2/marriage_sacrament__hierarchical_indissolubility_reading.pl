% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage Indissolubility — Hierarchical Adjudication Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   The canonical regime instantiated by this reading holds that a ratified
 *   and consummated sacramental marriage is an ontological bond dissolvable
 *   only by death, that only hierarchical authority (tribunals acting under
 *   episcopal and papal jurisdiction) may adjudicate marital status, and that
 *   the divorced-and-remarried therefore live outside full sacramental
 *   communion. The regime presents itself as fidelity to what marriage IS,
 *   not as a policy choice; its enforcement runs through denial of the
 *   Eucharist, tribunal gatekeeping of nullity, and the standing consequences
 *   that follow. KEY AGENTS (by structural relationship):
 *   curial_and_episcopal_hierarchy — primary beneficiary and agenda-setter
 *   (institutional/identity_locked), collects adjudicative authority and
 *   boundary control; canon_law_tribunal_establishment — secondary
 *   beneficiary-operator (organized/constrained), professional existence
 *   rides on the caseload; divorced_and_remarried_catholics — primary target
 *   (powerless/identity_locked), bears Eucharistic exclusion and standing
 *   loss; spouses_in_abusive_or_dead_marriages_denied_nullity — secondary
 *   target (powerless/trapped), cannot obtain canonical exit;
 *   faithful_married_couples_in_good_standing — coordinated beneficiaries
 *   (moderate/identity_locked), receive the credibility guarantee and
 *   subsidize with compliance; local_pastors_and_confessors — frontline
 *   administrators (moderate/identity_locked) absorbing pastoral fallout;
 *   orthodox_churches_oikonomia_practice — analytical contrast showing a
 *   functioning alternative; civil_divorce_regimes — excluded external actor
 *   keeping external alternatives alive. This story is ONE READING of the
 *   marriage_sacrament kernel; the sibling civic_pastoral_reading is a
 *   separate constraint file (linked via network.affects_constraints) and
 *   nothing of its framing is averaged into this story's epsilon. The epsilon
 *   referent is the standing hierarchical-indissolubility arrangement itself,
 *   assessed as this reading's own frame discloses it — including the
 *   exclusion costs the frame itself acknowledges imposing on real people.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.62).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage Indissolubility — Hierarchical Adjudication Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'f07ca332-df38-41a4-8cdd-cf5779bc59ee').
narrative_ontology:cs_kernel_codification('f07ca332-df38-41a4-8cdd-cf5779bc59ee', fixed_text).
narrative_ontology:cs_authority_grounding('f07ca332-df38-41a4-8cdd-cf5779bc59ee', lineage).
narrative_ontology:cs_interpretation_layer_present('f07ca332-df38-41a4-8cdd-cf5779bc59ee').
narrative_ontology:cs_reading_relation('f07ca332-df38-41a4-8cdd-cf5779bc59ee', marriage_sacrament__civic_pastoral_reading, forecloses).
narrative_ontology:cs_axiom('f07ca332-df38-41a4-8cdd-cf5779bc59ee', foundational, marriage_bond_ontologically_constitutive).
narrative_ontology:cs_axiom_status(marriage_bond_ontologically_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('f07ca332-df38-41a4-8cdd-cf5779bc59ee', marriage_bond_ontologically_constitutive, theological).
narrative_ontology:cs_axiom('f07ca332-df38-41a4-8cdd-cf5779bc59ee', foundational, hierarchical_adjudication_exclusive).
narrative_ontology:cs_axiom_status(hierarchical_adjudication_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('f07ca332-df38-41a4-8cdd-cf5779bc59ee', hierarchical_adjudication_exclusive, conventional).
narrative_ontology:cs_axiom('f07ca332-df38-41a4-8cdd-cf5779bc59ee', secondary, eucharistic_access_presupposes_marital_conformity).
narrative_ontology:cs_axiom_status(eucharistic_access_presupposes_marital_conformity, holdable).
narrative_ontology:cs_axiom_grounding('f07ca332-df38-41a4-8cdd-cf5779bc59ee', eucharistic_access_presupposes_marital_conformity, theological).
narrative_ontology:cs_reference_frame('f07ca332-df38-41a4-8cdd-cf5779bc59ee', constitutive_indissoluble_bond_order).
narrative_ontology:cs_drift_state('f07ca332-df38-41a4-8cdd-cf5779bc59ee', amoris_laetitia_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f07ca332-df38-41a4-8cdd-cf5779bc59ee', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, curial_and_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_establishment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, faithful_married_couples_in_good_standing).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_and_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, spouses_in_abusive_or_dead_marriages_denied_nullity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, local_pastors_and_confessors).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, indissolubility_constitutive_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_matrimony_ontology).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_jurisdiction_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the doctrine that a ratified and consummated sacramental marriage cannot be dissolved except by death, legislates the canonical process by which marital cases are examined, and directs bishops and tribunals in applying it. Every marital-status question in the global church passes through offices answerable to it, and the adjudicative authority the discipline concentrates accrues here. Its own legitimacy claim is fused with the discipline — conceding that valid marriages can end by discernment rather than death would unsettle the authority that adjudicates them — so stepping away from the position is not realistically available to it as an institution.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, curial_and_episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% Canonists, tribunal judges, and diocesan marriage-process offices receive petitions, gather evidence, conduct hearings, and issue declarations on marital validity. Their professional formation, staffing, and institutional weight depend on the volume and gravity of the caseload the discipline generates. They administer the process day to day under doctrine set elsewhere; leaving the field would mean leaving canon-law practice altogether.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_establishment, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_establishment, agenda_setter).

% Married within the discipline and living inside it, they receive what the arrangement promises: a union whose permanence is guaranteed by something larger than either spouse's will, communal recognition, and elevated standing relative to informal or serial partnerships. They subsidize the discipline with their compliance and sometimes with the pain of watching relatives excluded. Their marital identity was constituted inside this frame; leaving it would mean re-authoring their own history.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, faithful_married_couples_in_good_standing, beneficiary,
    moderate, generational, identity_locked, global).

% Civilly divorced and often remarried, they remain baptized members of the community but are barred from Eucharistic communion while the new union persists, unless a tribunal declares the first marriage null. Many continue attending Mass and abstain from the rail without ever being formally named; others petition tribunals and wait months or years for a decision. Joining a communion that permits remarriage would lift the bar at the price of the sacramental life and identity they were formed in.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_and_remarried_catholics, payer,
    powerless, biographical, identity_locked, global).

% Still canonically bound to marriages that have collapsed in fact — through violence, abandonment, or irretrievable breakdown — they petition for a declaration of nullity and are denied when evidence of invalidity cannot be established to the tribunal's standard. They cannot enter a new union the church would recognize, and entering one anyway costs them communion. Their way out of the bind depends entirely on an adjudication they do not control.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, spouses_in_abusive_or_dead_marriages_denied_nullity, payer,
    powerless, biographical, trapped, global).

% Parish priests apply the discipline at ground level: preparing couples for marriage, deciding in confession and at the rail how the bar is presented, accompanying the excluded. They carry a discretionary burden the center does not — the costs of the rule arrive in their confessional and at their altar — and their credibility with estranged parishioners rides on how they administer it. Ordination binds them to the system that assigns them this role.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, local_pastors_and_confessors, agenda_setter,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, local_pastors_and_confessors, payer).

% Eastern Orthodox churches, sharing apostolic orders and a sacramental theology of marriage, permit a penitential second (at most third) marriage after divorce through the economy (oikonomia) exercised by bishops. They stand as a working demonstration that a sacramental marriage discipline can accommodate post-divorce unions without tribunal nullity machinery, and they are cited constantly in intra-Catholic debate for exactly that reason.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, orthodox_churches_oikonomia_practice, observer,
    institutional, generational, analytical, continental).

% State legal systems dissolve marriages by civil decree under their own criteria, indifferent to canonical status. They sit wholly outside the adjudication conversation; their decrees manufacture the factual situations — divorce, remarriage — that the canonical machinery must then classify. Nothing the discipline does constrains them, and their continued operation is what keeps external alternatives alive for everyone inside.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civil_divorce_regimes, excluded,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, curial_and_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a credibility problem for long-horizon spousal commitment: by removing every sanctioned exit, the exchange of consent becomes maximally trustworthy to both spouses, their children, and the community. It also maintains a single sacramental boundary — who may approach the Eucharist — and provides one universal office for adjudicating marital-status questions.
% TRANSFER_FUNCTION: Moves standing, access, and deference rather than money: divorced and remarried faithful forgo sacramental access and communal standing; petitioners transfer time, documentation, and (until recently) fees to tribunals; the hierarchy collects adjudicative jurisdiction, boundary control over membership standing, and the legitimation that flows from administering a problem designed to have no easy exit.
% ABSENT_VOICES: Divorced and remarried Catholics attended the family synods as auditors and witnesses but held no deliberative vote; their testimony reached the floor filtered through clergy delegates. Civil legal systems and other Christian communions were outside the room entirely. The recorded unanimity of the final documents therefore arose in a conversation whose most affected class was present without voice.
% DISAPPEARANCE_RATIONALE: Tribunal offices, marriage-preparation programs, and the standing of millions of Catholics in irregular unions would reorganize immediately; the hierarchy would lose the adjudicative monopoly that anchors its jurisdiction over domestic life; ecumenical distance from Orthodox practice would narrow; and the credibility premium attached to sacramental marriage would migrate to whatever commitment devices replaced it.
% FOUNDING_PROBLEM: In a legal culture of unilateral male repudiation, the discipline was built to make spousal commitment maximally credible and to protect the abandoned party and children: if no power on earth can dissolve what God has joined, neither spouse can unilaterally exit, and the weaker party's security does not depend on the stronger's continuing affection.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of late-antique and medieval family law corroborate the original protective function from outside the benefiting parties, and contemporary family-law scholarship on no-fault divorce corroborates that the underlying vulnerability persists. Corroboration that THIS machinery — tribunal exclusivity and the Eucharistic bar — remains necessary to that problem comes almost entirely from the hierarchy and its apologists; no outside source attests the machinery's necessity, which is itself signal.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the regime's costs concentrate on a specific and growing class: as civil divorce spread through historically Catholic societies, the population living in 'irregular' unions expanded from a marginal case to a mass phenomenon, each member barred from communion or dependent on tribunal relief. Suppression (0.62) is a raw structural property, unscaled by power or scope: the sacramental bar and tribunal burden are structural, but real exits exist (other communions, secularity), and the 2015 process reforms lowered the wall — hence below the platform-commission grade. Theater ratio (0.36) reflects a process that does genuine adjudicative work but carries a large form-preserving share: nullity frequently functions as licensed dissolution that maintains doctrinal appearance while releasing individuals — the share peaked mid-interval when tribunal volume industrialized, then receded as reforms shortened the process. Accessibility collapse (0.68): inside the frame alternatives collapse nearly completely — no self-help, no second union, no communion — but civil and ecumenical exits keep external alternatives alive, so this is well short of natural-law grade. Resistance (0.55): persistent theological dissent, regional divergence in implementing recent reforms, and quiet dropout (lapsed affiliation) rather than organized confrontation. Coalition note: the victim class is enormous and geographically dense but shame-dispersed and identity-bound; its latent class power has never been organized, which is precisely why individually-powerless payers sustain the arrangement. The measurement trajectories are build-up-and-decay, not oscillation: enforcement intensified through the mid-century defense against spreading civil divorce, then decayed after 2000 as fees were abolished, the process shortened, and discernment pathways opened. The claimed type (tangled_rope) and the metrics are independently authored: the claim asserts a genuine coordination function (credible-commitment device, sacramental boundary, universal adjudication) coexisting with asymmetric extraction; the metrics describe the operation as observed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the hierarchy's position the arrangement is fidelity to an ontological reality it did not invent and cannot revise without dissolving its own adjudicative authority — institutional identity fusion: the authority claim and the discipline are the same thing, so the seat experiences no extraction at all, only costly faithfulness. From the payer seats the same structure is experienced as exclusion priced into their sacramental life, with exit available only at the cost of the identity the discipline formed. Local pastors straddle the two: they administer the rule and absorb its casualties in the same week. Orthodox observers see a functioning alternative that breaks the equation between sacramental marriage and absolute indissolubility. Civil regimes are untouched — the constraint governs an internal forum they never enter. If the hierarchy's identity frame broke (a formal concession that valid marriages can end by discernment), the enforcement machinery would collapse into the sibling reading's shape within a generation, because the tribunal monopoly and the Eucharistic bar both presuppose the constitutive claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The hierarchy sits nearest the beneficiary pole (declared beneficiary, agenda-setter, identity-locked exit that entrenches rather than exposes it). The tribunal establishment derives slightly higher d than the hierarchy: it collects jurisdiction and livelihood but also bears the caseload and the reputational exposure of the process it runs. Faithful married couples derive near-symmetric-low: genuine benefit (credibility guarantee, standing) offset by compliance costs and vicarious harm when kin are excluded. The two victim classes derive near the full-target pole, amplified by exit posture — identity_locked for the divorced-and-remarried (leaving costs them the sacramental universe constituting their identity), trapped for spouses denied nullity (no self-help path exists). Same-level divergence: divorced-and-remarried and faithful couples occupy adjacent lay standing but opposite directionalities, differentiated entirely by marital history relative to the tribunal's judgment — the constraint-specific factor, not global power, sets their positions. Pastors mix enforcement and payment and land mid-range. Orthodox observers and civil regimes sit outside the gain-and-cost flow altogether.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making spousal commitment credible and protecting the abandoned party when unilateral exit is available — is still partly live: no-fault divorce regimes generate real abandonment harms, and the credibility function of an exit-proof promise has not been superseded. But the specific machinery is contested: whether tribunal exclusivity and the Eucharistic bar are necessary to that problem is attested by no one outside the beneficiary set. The status-x-verdict pair (contested x world_rearranges) correctly avoids the dead-mandate zombie flag while the temporal series marks partial mandate erosion: the theater-ratio hump (0.22 to 0.46 and back to 0.36) records the period when process volume outran adjudicative substance, and the enforcement-decay slope after 2000 records the mandate narrowing to a rump defended by reaffirmation. The classification prevents both misreadings: a pure-extraction reading would erase the genuine commitment-device benefit accruing to compliant couples and the real historical protection of abandoned spouses; a pure-coordination reading would erase the concentrated exclusion costs, the tribunal burden, and the authority rents that flow to identifiable seats. The structure holds both, asymmetrically, under active enforcement — which is what the tangled-rope claim asserts and what the metrics independently describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the marriage_sacrament kernel (hierarchical_indissolubility_reading). What would adopting the sibling civic_pastoral_reading change structurally?',
    'Compare enforcement outcomes where the sibling''s practice operates — Orthodox oikonomia jurisdictions and post-Amoris discernment pathways: if Eucharistic exclusion and tribunal burden fall while community-cohesion indicators hold, the structural delta is confirmed.',
    'Sibling adoption would remove the Eucharistic bar and tribunal gatekeeping, cutting measured extraction sharply and shifting the structure toward a coordination arrangement carrying only a transitional enforcement residue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: what the sibling reading of the marriage_sacrament kernel would structurally change relative to this reading.').

omega_variable(
    tribunal_function_ambiguity,
    'Does the nullity process function as genuine truth-seeking about marital validity, or as a form-preserving legalization channel that dissolves marriages while maintaining doctrinal appearance?',
    'Distribution of grounds for nullity across eras, tribunal rejection rates, and longitudinal comparison of declared-null marriages against denied petitions.',
    'If legalization channel, the theater_ratio understates the performative share and the structure trends toward inertial performance; if genuine adjudication, the coordination function is stronger than the extraction-centered reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_function_ambiguity, empirical, 'Whether tribunal activity is substantive adjudication or licensed dissolution behind doctrinal form.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression experienced by divorced and remarried Catholics structural (sacramental bar, tribunal burden, standing loss) or internalized (shame-driven self-exclusion that persists independent of formal rules)?',
    'Post-relaxation trajectory: compare communion-line behavior and self-reported standing in parishes before and after the process reforms and discernment pathways; persistence of abstention after the formal bar eases indicates internalized carryover.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts disciplinary relaxation; relief then requires identity-level change, not rule change alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in the payer population.').

omega_variable(
    natural_law_substrate_question,
    'Does indissolubility track a natural-law structure of exclusive permanent pairing (a reality-like substrate the discipline merely names), or is it purely institutional construction maintained by enforcement?',
    'Cross-cultural comparative anthropology of marital-permanence norms and their stability absent enforcement, together with philosophical analysis of the natural-law claim itself.',
    'A genuine substrate would give the constraint a reality-like floor beneath the constructed enforcement layers; pure construction would make the entire structure contingent on institutional choice and revisable without remainder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_substrate_question, conceptual, 'Whether the discipline names a pre-institutional structure or constructs one.').

omega_variable(
    enforcement_decay_trajectory,
    'Will the post-2000 enforcement decay (abolished fees, shortened process, discernment pathways) continue until practice converges toward the sibling reading''s shape, or will it stabilize as a ratchet that periodically reverses?',
    'Track tribunal volumes, nullity rates, and subsequent synodal and curial documents over the next two decades; reversal markers include renewed procedural requirements or disciplinary reaffirmations.',
    'Continued decay drives theater_ratio upward and extraction downward along an inertial drift; reversal restores the enforcement ratchet and the mid-interval profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Direction of the enforcement-capacity trend the measurement series currently shows decaying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1917, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1917, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1917, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1950, 0.27).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1970, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement_basis(marr_tr_t1970, observed).
narrative_ontology:measurement(marr_tr_t1983, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1983, 0.43).
narrative_ontology:measurement_basis(marr_tr_t1983, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2000, 0.46).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2016, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2016, 0.41).
narrative_ontology:measurement_basis(marr_tr_t2016, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(marr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1917, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1917, 0.54).
narrative_ontology:measurement_basis(marr_be_t1917, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1970, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1970, 0.67).
narrative_ontology:measurement_basis(marr_be_t1970, observed).
narrative_ontology:measurement(marr_be_t1983, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1983, 0.71).
narrative_ontology:measurement_basis(marr_be_t1983, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2016, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2016, 0.76).
narrative_ontology:measurement_basis(marr_be_t2016, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2026, 0.72).
narrative_ontology:measurement_basis(marr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1917, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1917, 0.58).
narrative_ontology:measurement_basis(marr_su_t1917, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1950, 0.64).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1970, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1970, 0.71).
narrative_ontology:measurement_basis(marr_su_t1970, observed).
narrative_ontology:measurement(marr_su_t1983, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1983, 0.73).
narrative_ontology:measurement_basis(marr_su_t1983, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2016, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2016, 0.64).
narrative_ontology:measurement_basis(marr_su_t2016, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(marr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: the colloquial label 'Catholic marriage discipline' covers two structurally distinct constraints. This story authors epsilon for the standing hierarchical-indissolubility arrangement (high: concentrated exclusion costs, tribunal burden, authority rents). The sibling marriage_sacrament__civic_pastoral_reading authors epsilon for the discernment-based arrangement it embodies where practiced (low: exclusion relieved, coordination retained as accompaniment). The hierarchical reading is upstream and historically established; it conditions the sibling's operating environment — what pastoral accommodation is canonically permissible — hence the affects_constraints edge. The attachment-coordination layer of marriage norms generally is a further distinct constraint, deliberately not modeled here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
