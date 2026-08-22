% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_individual_right_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Constitutional Shield for Private Arms Possession
 *   domain: legal/constitutional/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   individual_right_reading of the Second Amendment, on which the right to
 *   keep and bear arms is an individual liberty pre-existing government,
 *   protected against federal infringement and, since incorporation, against
 *   state infringement. The constraint under classification is the standing
 *   arrangement that reading produces in operation: a constitutional shield
 *   over private arms possession, administered by courts through
 *   text-history-tradition review, binding legislative and executive bodies
 *   at both levels of government. The reading presents the right as natural
 *   law (the constraint's own content asserts pre-existence), yet the
 *   arrangement has identifiable, concentrated beneficiaries and
 *   identifiable, trapped cost bearers, and it meets among the heaviest
 *   sustained resistance of any doctrine in the American constitutional
 *   order. The claimed_type below is the reading's own naturality assertion,
 *   authored faithfully; the metrics are independent descriptive judgments of
 *   how the arrangement actually operates. That gap is the false-summit
 *   signal this corpus exists to take, and it is authored, not reconciled.
 *   KEY AGENTS (by structural relationship): - individual_gun_owners: primary
 *   beneficiary (organized/identity_locked) — constitutionally shielded
 *   possession - firearms_manufacturers_and_dealers: concentrated commercial
 *   beneficiary (powerful/arbitrage) - gun_rights_advocacy_organizations:
 *   institutional beneficiary (organized/identity_locked) -
 *   federal_judiciary: agenda setter (institutional/constrained) —
 *   administers the boundary - federal_legislative_and_executive_branches:
 *   constrained payer (institutional/constrained) -
 *   state_governments_enacting_regulation: constrained payer
 *   (institutional/constrained) - communities_bearing_gun_violence_burden:
 *   diffuse cost bearer (powerless/trapped) -
 *   gun_violence_survivors_and_bereaved_families: direct cost bearer
 *   (powerless/trapped) - gun_control_advocacy_movement: excluded voice
 *   (organized/constrained) - constitutional_scholars_and_historians:
 *   analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.3).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading: Constitutional Shield for Private Arms Possession").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "legal/constitutional/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).
domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'b7361daf-8a5e-4693-9239-5d159c6fa38f').
narrative_ontology:cs_kernel_codification('b7361daf-8a5e-4693-9239-5d159c6fa38f', fixed_text).
narrative_ontology:cs_authority_grounding('b7361daf-8a5e-4693-9239-5d159c6fa38f', lineage).
narrative_ontology:cs_interpretation_layer_present('b7361daf-8a5e-4693-9239-5d159c6fa38f').
narrative_ontology:cs_reading_relation('b7361daf-8a5e-4693-9239-5d159c6fa38f', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b7361daf-8a5e-4693-9239-5d159c6fa38f', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('b7361daf-8a5e-4693-9239-5d159c6fa38f', foundational, arms_possession_individual_preexisting_liberty).
narrative_ontology:cs_axiom_status(arms_possession_individual_preexisting_liberty, holdable).
narrative_ontology:cs_axiom_grounding('b7361daf-8a5e-4693-9239-5d159c6fa38f', arms_possession_individual_preexisting_liberty, deontological).
narrative_ontology:cs_axiom('b7361daf-8a5e-4693-9239-5d159c6fa38f', secondary, individual_armament_secures_liberty_against_tyranny).
narrative_ontology:cs_axiom_status(individual_armament_secures_liberty_against_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('b7361daf-8a5e-4693-9239-5d159c6fa38f', individual_armament_secures_liberty_against_tyranny, instrumental).
narrative_ontology:cs_reference_frame('b7361daf-8a5e-4693-9239-5d159c6fa38f', preexisting_natural_individual_liberty).
narrative_ontology:cs_drift_state('b7361daf-8a5e-4693-9239-5d159c6fa38f', contemporary_post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b7361daf-8a5e-4693-9239-5d159c6fa38f', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_manufacturers_and_dealers).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, communities_bearing_gun_violence_burden).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_violence_survivors_and_bereaved_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_legislative_and_executive_branches).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_governments_enacting_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own firearms for self-defense, hunting, sport, and collection. The arrangement shields their acquisitions and carry practices from federal prohibition and, since incorporation, from state prohibition; courts strike laws they could not defeat legislatively. For a large subset, possession is fused with political and personal identity, so divesting is culturally and psychologically costly and exit from the protected category is rarely taken even as circumstances change.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufacture and sell into a demand curve the arrangement helps stabilize: a constitutionally protected product category with a growing customer base. A 2005 liability statute bars most civil suits arising from criminal misuse of their products, so the external costs of widespread ownership do not return to them as damages. They fund advocacy litigation and lobbying that defend the arrangement's boundaries; exit means retooling product lines or shifting to foreign markets, which remains available to them.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_manufacturers_and_dealers, beneficiary,
    powerful, biographical, arbitrage, national).

% Litigate, lobby, and mobilize around the arrangement's expansion. Membership, donations, and institutional purpose track the perceived threat to the right, so perpetual contest sustains them materially. Their organizational identity is fused with the cause: dissolution of the contest would dissolve them.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Administers the boundary: decides which arms regulations survive text-history-tradition review, strikes others, and defines the right's outer limits. Lifetime tenure insulates judges from electoral consequence, but the docket cannot be declined and precedents bind future panels. The seat collects no material rent from the arrangement; its stake is doctrinal.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Draft and enact arms regulation inside a narrowing zone of constitutional permission. Measures that pass majorities are voided by courts, and officials face damages suits for enforcing invalidated rules. The Article V exit — amendment — requires supermajorities that the beneficiary coalition reliably withholds. They bear the constraint as foreclosed policy capacity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_legislative_and_executive_branches, payer,
    institutional, generational, constrained, national).

% Bound by the same rule since incorporation; after 2022, discretionary carry-licensing regimes fell and permitless carry spread. States redesign statutes to survive historical-analogue review, absorb litigation losses, and carry the fiscal costs of both compliance and challenge. Exit is unavailable short of the same federal amendment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_governments_enacting_regulation, payer,
    institutional, generational, constrained, regional).

% Concentrated urban and disproportionately low-income communities where firearm saturation translates into elevated homicide and suicide exposure across generations. They did not consent to the arrangement, gain little from it, and cannot relocate out of national gun prevalence. Their preferred remedies are precisely the ones the arrangement forecloses, and their political weight has not moved the adjudicative seat.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, communities_bearing_gun_violence_burden, payer,
    powerless, generational, trapped, regional).

% Direct casualties of the arrangement's externality: the injured and the families of the killed. Compensation routes are closed — the liability shield bars most suits against the industry, and the arrangement offers no remedy channel of its own. Exit is meaningless for them; the cost is already borne.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_survivors_and_bereaved_families, payer,
    powerless, biographical, trapped, local).

% Organized opinion representing repeated majority support for measures the arrangement invalidates. They argue in legislatures and referenda and win there, then lose in the forum that binds. They are inside the political conversation and outside the operative interpretive one: the adjudicative register — text, history, tradition — is one in which their premises carry no standing.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocacy_movement, excluded,
    organized, generational, constrained, national).

% Professional historians and legal scholars who audit the arrangement's historical claims. Much of the historical profession finds the pre-existence narrative and the founding-era analogies deployed in doctrine to be selective reconstructions; their findings circulate widely and alter nothing binding. From this seat the full structure is visible: a constructed text, lineage authority, identity-fused beneficiaries, and trapped cost bearers.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars_and_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, firearms_manufacturers_and_dealers).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a durable collective-action problem: individual armament cannot protect itself against shifting legislative majorities one statute at a time, so a supermajority-entrenched constitutional rule binds future governments at once. It also preserves a distributed defensive capacity — an armed citizenry — which the founding generation treated as a check on standing armies and which this reading carries forward as a liberty guarantee.
% TRANSFER_FUNCTION: Moves regulatory discretion out of legislative and executive hands into a constitutionally reserved private sphere, with courts arbitrating the boundary; moves the liability costs of product misuse off manufacturers and onto the public; moves primary security provisioning from public institutions to individual households.
% ABSENT_VOICES: The communities and families bearing the violence burden object from outside the interpretive forum: the adjudicative register — text, history, tradition — gives their premises no standing, so their objection never reaches the seat that decides. The founding-era militia populace whose consent the historical record invokes cannot testify to whether they would recognize universal personal handgun carry as their arrangement. The dead are absent without proxy.
% DISAPPEARANCE_RATIONALE: Overnight disappearance reopens the entire regulatory field: federal and state prohibition and licensing regimes would proliferate within legislative sessions, tens of millions of lawful possessions would become contingent on permits or bans, the industry's protected domestic market would contract sharply, and the advocacy ecosystem on both sides would reorganize around the new battlefield. Arrangements this many parties are organized around do not vanish neutrally.
% FOUNDING_PROBLEM: The 1791 settlement answered a specific fear: a federal standing army disarming the state militias, converting the republic's defensive body into a federal monopoly. The arrangement guaranteed that the people's arms, organized through the militia system, would remain a check on central force held by the states and the people.
% FOUNDING_PROBLEM_CORROBORATION: Militia-era historiography and the professional historical consensus — sources outside the beneficiary coalition — attest that the founding problem was militia-federalism and that the 1903 National Guard Act and the permanent standing military dissolved its object; the Heller dissent attests the same reading from inside the legal tradition. The reading's own adherents attest that the problem lives on transmuted as natural-liberty protection. No disinterested source corroborates the perpetuity framing; the external corroboration that exists supports the transformation reading, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon's referent is the standing arrangement itself — the individual-right doctrine in operation — assessed by this reading's own lights, hence the moderate scalar (0.30): the reading counts restraint-on-government as the justified price of a pre-existing right, concedes the right is not absolute, and cannot zero out enforcement and boundary-drawing costs. Prohibition measures are a DIFFERENT constraint: they are what this arrangement strikes down, and under this reading they carry high epsilon as infringements — they belong in their own stories, linked through the network, not folded into this one (epsilon-invariance decomposition). Suppression (0.62) is authored as a raw structural property, unscaled by power or scope: the arrangement actively nullifies democratically enacted regulation through court orders and official liability, and its Article V exit is effectively closed. Resistance (0.78) is among the heaviest met by any constitutional doctrine; accessibility_collapse (0.58) is partial — the interpretive alternative collapsed legally after 2008, but regulatory alternatives remain live inside the doctrine's bounds. Theater (0.22) tracks the growth of performative originalism: historical-analogue lawyering and naturality rhetoric layered over a functional enforcement core. Interval mapping: T0 = 2008 (Heller announces the reading as binding law), T17 = 2025 (mature post-Bruen era); all three tracked series share the single grid {0,3,6,9,12,15,17}. Base extractiveness rises gently because the restraint's SCOPE grew (incorporation, then the 2022 carry right) even as the reading's assessment of its legitimacy stayed constant. Suppression_requirement rises steeply because the story specifically traces enforcement-capacity build-up: from a handful of invalidated ordinances to hundreds of challenges, injunction practice, and damages actions against officials. Theater rises as the history-method's evidentiary burden invited ritualized historical citation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structural data. From the owner seat the arrangement is subsidy: a shield that lowers the price of possession, with identity lock making exit unthinkable — a coordination good. From the legislative and executive seats it is targeting: foreclosed policy capacity under active judicial enforcement. From the violence-burdened community and survivor seats it is the harshest position in the story: severe, uncompensated, unconsented burden with no exit and no remedy channel. The judiciary's administrative seat sits between: it expends resources and owns the boundary but collects no rent. The reading's own seat claims mountain — pre-existing natural law — while the structural data (active enforcement, concentrated beneficiaries, trapped cost bearers, heavy resistance) supports a hybrid profile. The engine computes this divergence per seat; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: owners, industry, and advocacy organizations sit near the subsidized pole (d approximately 0.05-0.15). Identity lock on owners does not push them toward the target pole — lock modulates exit, and their structural position is receipt of protection, not payment. The victim declarations map to high directionality: trapped, uncompensated communities and bereaved families sit near the full-target pole (d approximately 0.85-0.95), their preferred remedies being precisely what the arrangement forecloses. The legislative and executive payers derive high as well: foreclosed capacity is the constraint's operating cost made institutional, with the amendment exit withheld by the beneficiary coalition itself. The judiciary, as administering seat, derives mid-range: it spends enforcement resources and owns the boundary but collects no material gain. Scope amplification applies modestly at national scale, hardest on the verification of externalized costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's founding problem — a federal standing army disarming the state militias — was materially dissolved by the National Guard Act of 1903 and the permanence of the standing military; the militia-system object the 1791 settlement protected no longer exists in its founding form. The reading answers that transformation by re-grounding the mandate in natural-liberty perpetuity: a successor justification, not the founding one. mandatrophy_resolved is therefore declared true — the original mandate is obsolete even as the arrangement thrives — while founding_problem_status is authored contested, because the parties genuinely dispute whether the problem lives on transmuted or is dead with the militia. Classification discipline cuts both ways: it prevents mislabeling the owners' genuine liberty-coordination as pure extraction (their seat really is subsidized, and millions hold the shield as a real good), and it prevents the naturality rhetoric from laundering the enforced foreclosure and the externalized costs (the engine prices those at the seats that actually bear them). The receipt surface records where the arrangement's gains demonstrably land: the industry seat converts foreclosed regulation and the liability shield into concentrated protected revenue, which is receipt, distinct from the diffuse liberty benefit owners collect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_right,
    'Is the individual right genuinely a pre-existing natural liberty that government merely recognizes, or a constructed constitutional settlement presented as pre-political nature?',
    'Comparative documentary record: pre-1791 state constitutions, the English Bill of Rights lineage, ratification and militia-act history, and whether any jurisdiction treated arms possession as legally prior to constitution-making.',
    'If constructed, the mountain claim fails and the arrangement classifies as an enforced hybrid with concentrated beneficiaries; if natural, the restraint-on-government framing strengthens and extraction attributions weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_right, conceptual, 'Naturality of the right versus constructed-settlement presentation.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the individual_right_reading of the second_amendment_arms_right kernel: what would the sibling readings change structurally, and where is the disagreement located?',
    'Instantiating collective_right_reading removes individual owners from the beneficiary set and releases regulatory authority; instantiating civic_republican_reading re-centers duties of armed citizenship and conditions the right on civic participation. The disagreement sits in the right-holder and in the pre-existence axiom.',
    'Beneficiary and victim sets and the resulting directionality derivations flip across readings; cross-reading comparison is valid only reading-by-reading, never averaged into one epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings, disagreement located in right-holder and pre-existence.').

omega_variable(
    externality_causal_attribution,
    'Are the violence burdens borne by exposed communities attributable to this arrangement''s protection of widespread ownership, or to criminal behavior that would persist under any regime?',
    'Quasi-experimental evidence: post-2022 carry expansion staggered across states, shall-issue adoption histories, and ownership-density versus gun-homicide panels controlling for poverty and policing intensity.',
    'Attribution to the arrangement validates the victim declarations and drives the burdened seats toward the full-target pole; attribution to independent criminality weakens the victim set and pulls the arrangement toward a defended-liberty coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_causal_attribution, empirical, 'Causal chain from protected ownership to community burden.').

omega_variable(
    owner_identity_lock_depth,
    'How deeply is gun ownership fused with owner identity such that exit remains unthinkable even as costs accumulate?',
    'Longitudinal owner-attrition data across political and cultural shocks; survey panels comparing divestment intent with actual divestment behavior.',
    'Deep identity lock stabilizes the beneficiary coalition indefinitely and raises persistence independent of function; shallow lock makes the coalition responsive to accumulating costs and loosens the arrangement''s grip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(owner_identity_lock_depth, empirical, 'Identity-fusion depth of the owner beneficiary seat.').

omega_variable(
    regulatory_chilling_mechanism,
    'Is the suppressed regulatory response structural (courts void enacted laws) or internalized (officials preemptively decline to legislate in anticipation of invalidation)?',
    'Compare introduced and enacted arms-regulation volume before and after 2022, controlling for issue salience and partisan composition; code legislative materials for anticipated-invalidation rationales.',
    'An internalized component means effective suppression exceeds the formal enforcement measure: the chill travels with officials even into doctrinal spaces where regulation remains permitted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chilling_mechanism, empirical, 'Structural versus internalized suppression of regulatory alternatives.').

omega_variable(
    history_method_evidentiary_reliability,
    'Does text-history-tradition review reliably constrain outcomes, or does it supply discretionary cover in which nearly any result can find analogues?',
    'Inter-rater reliability studies of trained historians applying the method blind to desired outcomes; reversal-rate analysis of trial-court historical rulings on appeal.',
    'Low reliability converts the interpretive layer into performance and raises the theater ratio; high reliability supports a principled-lineage reading of the arrangement''s boundary administration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(history_method_evidentiary_reliability, empirical, 'Reliability of the historical-analogue method beneath the arrangement''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sm_individual_right_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sm_individual_right_tr_t3, second_amendment_arms_right__individual_right_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(sm_individual_right_tr_t6, second_amendment_arms_right__individual_right_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(sm_individual_right_tr_t9, second_amendment_arms_right__individual_right_reading, theater_ratio, 9, 0.16).
narrative_ontology:measurement(sm_individual_right_tr_t12, second_amendment_arms_right__individual_right_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(sm_individual_right_tr_t15, second_amendment_arms_right__individual_right_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(sm_individual_right_tr_t17, second_amendment_arms_right__individual_right_reading, theater_ratio, 17, 0.22).

% Extraction over time
narrative_ontology:measurement(sm_individual_right_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sm_individual_right_be_t3, second_amendment_arms_right__individual_right_reading, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(sm_individual_right_be_t6, second_amendment_arms_right__individual_right_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(sm_individual_right_be_t9, second_amendment_arms_right__individual_right_reading, base_extractiveness, 9, 0.27).
narrative_ontology:measurement(sm_individual_right_be_t12, second_amendment_arms_right__individual_right_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(sm_individual_right_be_t15, second_amendment_arms_right__individual_right_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(sm_individual_right_be_t17, second_amendment_arms_right__individual_right_reading, base_extractiveness, 17, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(sm_individual_right_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sm_individual_right_su_t3, second_amendment_arms_right__individual_right_reading, suppression_requirement, 3, 0.46).
narrative_ontology:measurement(sm_individual_right_su_t6, second_amendment_arms_right__individual_right_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(sm_individual_right_su_t9, second_amendment_arms_right__individual_right_reading, suppression_requirement, 9, 0.53).
narrative_ontology:measurement(sm_individual_right_su_t12, second_amendment_arms_right__individual_right_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(sm_individual_right_su_t15, second_amendment_arms_right__individual_right_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(sm_individual_right_su_t17, second_amendment_arms_right__individual_right_reading, suppression_requirement, 17, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% One ratified text, three readings, three constraints. This file instantiates the individual_right_reading; collective_right_reading and civic_republican_reading are separate stories with their own beneficiary sets, victim sets, and reading-indexed epsilon values over the shared referent. The structural pressure runs from this reading outward: its 2008-2022 victories reset the legitimacy conditions under which the siblings operate, absorbing the civic-republican militia rationale as justification while foreclosing the collective reading's core premise within any single framework. Prohibition-measure regimes — the laws this arrangement strikes down — are a further decomposition layer: under this reading they carry high epsilon as infringements of a pre-existing right, and they belong in their own stories rather than folded into this one, since assigning them a different epsilon than the shield arrangement is a sign they are different constraints, not a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
