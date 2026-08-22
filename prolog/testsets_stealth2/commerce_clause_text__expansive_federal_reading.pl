% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Aggregate-Effects Reading of the Federal Commerce Power
 *   domain: constitutional law/federalism/commerce regulation
 *
 * SUMMARY:
 *   The constraint under story is the doctrinal rule that the federal
 *   commerce power reaches any economic activity whose effects, aggregated
 *   across many actors, are substantial in national markets — the reading
 *   through which Congress regulates labor conditions, farm production,
 *   environmental quality, financial markets, and public accommodations
 *   wholly inside single states. It instantiates the
 *   expansive_federal_reading of the kernel commerce_clause_text; two sibling
 *   files instantiate the rival readings, and this file links them via
 *   network.affects_constraints per the epsilon-invariance decomposition rule
 *   (the colloquial label 'the Commerce Clause' covers three structurally
 *   distinct claims with distinct epsilon values and distinct party
 *   structures). Structurally the arrangement coordinates — one rulebook for
 *   a continental market, dissolving the state-tariff and
 *   regulatory-fragmentation problem — while simultaneously transferring
 *   regulatory authority upward, with identifiable payers (state and local
 *   institutions whose police powers are subordinated, and local
 *   constituencies whose divergent conditions are overridden by national
 *   aggregates) and identifiable collectors (federal institutions whose
 *   jurisdiction, budgets, and dockets expand with each successful
 *   application). I therefore CLAIM tangled_rope: genuine coordination
 *   function plus asymmetric extraction, held together by active enforcement.
 *   The METRICS are authored independently as descriptive estimates of actual
 *   operation; where the engine's per-seat computation diverges from this
 *   claim, that divergence is the datum. The epsilon referent is the standing
 *   arrangement — the existing federal regulatory domain resting on
 *   aggregate-effects justification — assessed by this reading's own lights:
 *   this reading accepts federal primacy as legitimate, yet honestly
 *   registers the continuing, real costs borne by subordinated seats,
 *   yielding moderate-high epsilon rather than the near-zero a triumphalist
 *   account of its own doctrine would assert.
 *
 * KEY AGENTS:
 *   - federal_judiciary: primary agenda-setter (institutional power / constrained exit) — defines and maintains the reading; cannot decline the disputes that define it
 *   - united_states_congress: co-agenda-setter and incidental beneficiary (institutional / arbitrage) — legislates within the enlarged space, routing around limits via spending and taxation when convenient
 *   - federal_administrative_agencies: primary beneficiary (institutional / arbitrage) — receive jurisdiction, budgets, and enforcement mandate with each expansion
 *   - large_interstate_firms: dual-positioned beneficiary-payer (powerful / constrained) — buy uniformity, absorb federal compliance costs, lobby for preemption of stricter state law
 *   - civil_rights_and_labor_organizations: coordination-function beneficiary (organized / constrained) — depend on the expansive reading as the vehicle for nationwide protections
 *   - state_governments: primary target (institutional / trapped) — police powers subordinated; union membership forecloses exit
 *   - local_governments: target (moderate / trapped) — ordinances and standards preempted from above
 *   - local_economic_actors: excluded voices bearing diffuse costs (powerless / trapped)
 *   - federalism_scholars: analytical observer — maps the boundary contests without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.66).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.56).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Aggregate-Effects Reading of the Federal Commerce Power").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional law/federalism/commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'eb0a4dfb-8891-44b1-8db8-e5ea550109a6').
narrative_ontology:cs_kernel_codification('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', fixed_text).
narrative_ontology:cs_authority_grounding('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', lineage).
narrative_ontology:cs_interpretation_layer_present('eb0a4dfb-8891-44b1-8db8-e5ea550109a6').
narrative_ontology:cs_reading_relation('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_reading_relation('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', foundational, aggregate_effects_within_interstate_commerce).
narrative_ontology:cs_axiom_status(aggregate_effects_within_interstate_commerce, holdable).
narrative_ontology:cs_axiom_grounding('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', aggregate_effects_within_interstate_commerce, empirically_contingent).
narrative_ontology:cs_axiom('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', foundational, no_nexus_or_pretext_qualifier_required).
narrative_ontology:cs_axiom_status(no_nexus_or_pretext_qualifier_required, holdable).
narrative_ontology:cs_axiom_grounding('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', no_nexus_or_pretext_qualifier_required, conventional).
narrative_ontology:cs_reference_frame('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', unlimited_substantial_aggregate_effects_scope).
narrative_ontology:cs_drift_state('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', contemporary_post_lopez_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb0a4dfb-8891-44b1-8db8-e5ea550109a6', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, united_states_congress).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, large_interstate_firms).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, civil_rights_and_labor_organizations).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, large_interstate_firms).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, demand_interdependence_economics).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_market_uniformity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and maintains the doctrinal line separating federal from state regulatory authority, drawing the aggregate-effects boundary case by case and policing it through preemption review. Gains docket centrality and institutional weight as the domain enlarges. Cannot decline the disputes that define the boundary; bound by its own precedent chain; its exit is limited to doctrinal adjustment, which it exercises episodically (Lopez, Morrison) before restoring the frame (Raich).
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_judiciary, beneficiary).

% Enacts statutes justified by aggregate-effects findings, expanding the legislative domain with each accepted reading. Answers to national electoral coalitions rather than to states as such. Holds arbitrage-grade flexibility: where the commerce route narrows, it reroutes through taxing, spending, and enforcement powers, so boundary setbacks redirect rather than restrain it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, united_states_congress, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, united_states_congress, beneficiary).

% Receive jurisdiction, staffing, budgets, and enforcement mandate with every successful application of the reading; draft the implementing rules; defend their scope in litigation. Can reinterpret mandates, reclassify regulated activity, and migrate between mission areas, giving them the most maneuverable position of any seat in the arrangement.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Buy regulatory uniformity: one federal rulebook replaces fifty-state patchwork compliance, and federal preemption conveniently displaces stricter state standards. Simultaneously absorb federal compliance costs and occasionally litigate against specific statutes. Cannot exit the national market they operate in; relocation of incorporation or headquarters does not escape federal reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, large_interstate_firms, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, large_interstate_firms, payer).

% Depend on the expansive reading as the constitutional vehicle for nationwide protections — public-accommodations guarantees, workplace standards, environmental floors — that no state-by-state strategy could secure against interstate undercutting. Under a narrower reading their policy portfolio loses its principal legal foundation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, civil_rights_and_labor_organizations, beneficiary,
    organized, generational, constrained, national).

% Hold residual police powers that contract whenever the aggregate-effects line moves outward; implement federally designed programs, frequently at their own expense; litigate the boundary through Tenth Amendment and anti-commandeering suits with a poor win record. Union membership is constitutionally irrevocable — no exit exists at any price — so their recourse is confined to litigation, intergovernmental coalitions, and political pressure.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    institutional, generational, trapped, regional).

% Municipal ordinances, land-use rules, and consumer or labor standards are displaced by federal floors and ceilings; their regulatory voice reaches Washington only filtered through state leagues and associations. No exit path exists; their authority is a creation of state law beneath a federal superstructure.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_governments, payer,
    moderate, biographical, trapped, local).

% Small businesses and residents whose local conditions diverge from national aggregates are governed by rules calibrated to the average, made by institutions remote from them. Their formal participation channel — notice-and-comment in federal rulemaking — is one they rarely use effectively. Would object to the terms of the arrangement if seated, but appear in the process chiefly as regulated subjects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_actors, excluded,
    powerless, biographical, trapped, local).

% Map the boundary contests, document the gap between the founding settlement and current doctrine, and supply the analytical record on which both defenders and critics of the arrangement draw. Collect nothing from the constraint's operation and pay nothing into it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federalism_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single rule-making authority for economic activity whose effects aggregate across state lines — replacing fifty-way regulatory fragmentation and suppressing state-level beggar-thy-neighbor regulation of a continental market.
% TRANSFER_FUNCTION: Moves regulatory authority — with the discretion, staffing, budgets, and enforcement resources attached to it — from state legislatures and agencies up to Congress and federal agencies; moves compliance standardization onto firms; moves decisions about local economic conditions from proximate institutions to distant ones.
% ABSENT_VOICES: State legislators and municipal officials hold no seat in the adjudication that defines the boundary — they enter only as litigants and amici after the line is drawn. Residents and small enterprises whose conditions diverge from the national aggregate participate, if at all, through notice-and-comment processes they rarely use; their objection to being governed by averages is structurally unrepresented.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the enumerated-power foundation of vast federal statutes — labor standards, environmental regulation, financial oversight, public-accommodations guarantees — evaporates. Congress re-legislates what it can under taxing and spending powers; states reassert police power over the remainder; the national market reorganizes around either a constitutional amendment or a fifty-state patchwork with new interstate frictions. Every named seat's arrangements depend on the current structure.
% FOUNDING_PROBLEM: Under the Articles of Confederation, states erected tariffs, discriminated against sister-state goods, and pursued commercial war with one another; the Framers built a national government capable of governing a continental market and preventing interstate trade barriers.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era historians, writing outside any beneficiary set, corroborate the original tariff-war problem and the design intent behind the commerce grant; transport and trade economists corroborate that genuinely interstate externalities exist and have grown with market integration. Against this, state officials and federalism scholars — also outside the benefiting parties — attest that much modern use addresses problems of a kind and scale the founding settlement never contemplated, serving administrative expansion rather than the founding problem. Both attestations come from outside the beneficiary set; the status is contested on independent testimony, not self-assertion.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: substantial and rising across the interval — each doctrinal success converts another activity class into federal jurisdiction, and the transfer is asymmetric (authority moves up; compliance standardization moves sideways onto firms; unfunded obligations move down onto states). Suppression 0.56 is authored as a RAW structural property and is never scaled — the coercive element is judicial boundary-policing and preemption, not participant preference. Theater_ratio 0.30: most federal regulatory activity under this reading is functionally real, but a persistent minority is performative federalism — consultation rituals, advisory bodies, and partnership framing that decorate decisions already taken. Accessibility_collapse 0.70: once the reading is accepted, state-level alternatives in covered domains collapse structurally (preemption removes them regardless of state preference), though genuine residual domains remain, keeping this below mountain-grade closure. Resistance 0.62: sustained — state attorney-general coalitions, Tenth Amendment litigation, the unfunded-mandate revolts of the 1990s, and recurring Court retrenchment attempts. Temporal grid is SHARED across all three tracked metrics at t = {0,16,32,48,64,80}, mapped to 1942 (Wickard), 1958, 1974, 1990 (Lopez pending), 2006 (post-Raich), 2022. The series are CYCLICAL, not monotonic: extractiveness dips at t=48 as the Rehnquist Court's federalism revival (Lopez, Morrison) forces retrenchment, then recovers (Raich); suppression_requirement oscillates in phase — heavy enforcement during establishment and during state-pushback eras, lighter during normalized consensus. The oscillation's driver is alternating federalism-revival waves meeting entrenched federal practice; part of its function is stabilization — periodic symbolic retrenchment operates as a safety valve that legitimizes the overall arrangement (a mild intermittent-concession mechanism), which is why the cycle is documented rather than smoothed away.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seats the same structure is the machinery of legitimate national governance: the judiciary experiences it as the disciplined application of a workable standard; Congress as democratic responsiveness to national-scale problems; agencies as mandate fulfillment. From the state and local seats the identical structure is experienced as subordination — rules made by distant majorities, calibrated to aggregates that erase local conditions, enforced through preemption with no reciprocal channel. The sharpest divergence sits among nominally equal actors: state governments and federal agencies both hold institutional power, yet agencies enjoy arbitrage-grade exit (reinterpret, reclassify, migrate missions) while states are constitutionally trapped (no secession, Tenth Amendment remedies rejected); large firms are powerful but merely constrained, able to relocate operations but not to exit the national market their lobbying helped integrate. Exit-option asymmetry, not global standing, generates the seat divergence the engine computes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: agencies accrue jurisdiction and budgets (lowest d); Congress and the judiciary accrue legislative territory and doctrinal docket respectively (low d, moderated by their administrator roles); firms accrue uniformity gains while paying real compliance costs (dual-positioned, mid-low d); civil-rights and labor organizations accrue the enforceability of nationwide protections (low d). Victim declarations map to the transfer's debit side: state governments bear the largest structural loss — police-power authority ceded without compensation — and, being trapped, sit near the full-target end (highest d); local governments bear preemption of their ordinances; local economic actors bear the diffuse cost of having their conditions overridden by national averages. Scope amplification applies: the arrangement operates at national scope, where verification of genuine aggregate effects is hardest, modestly raising effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading this as a pure snare would erase why subordinated seats nonetheless acquiesce: states receive federal funds, market integration, and genuine interstate-externality management they could not deliver alone — the coordination half is real and historically was the point. Reading it as a pure rope would erase the extraction layer: unfunded mandates, preemption of stricter local standards, and jurisdiction defended for its own sake by the institutions that hold it. The R5 genealogy sharpens this: the FOUNDING problem (state tariff wars under the Articles) is dead in its original form — no state can erect the barred tariffs — while successor problems (continental externalities) are contested-live. The arrangement persists partly through live function and partly through beneficiary defense and institutional inertia; the founding_problem_status='contested' x disappearance_verdict='world_rearranges' cell correctly reports that the world depends on the arrangement while disputing whether today's version still answers the reason it was built. Mandatrophy is therefore monitored, not declared resolved: the waiver-based natural-experiment omega is the instrument that would detect the drift toward piton if state-level performance under delegated autonomy proves comparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (expansive_federal_reading) of the kernel commerce_clause_text — what structural facts would change if a sibling reading were adopted instead?',
    'No data resolves this; it is the location of the live constitutional dispute. Compare compiled classifications across the sibling files (originalist_narrow_reading, substantial_effects_limited_reading).',
    'Under the originalist narrow reading the entire federal regulatory domain built on aggregate effects becomes ultra vires and the beneficiary/victim sets invert; under the substantial effects limited reading the domain survives but with entry costs (nexus, non-pretext) that shrink it. Classification of the same institutional arrangement is reading-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; disagreement located in the extension of ''Commerce...among the several States.''').

omega_variable(
    substantiality_threshold,
    'What quantum of aggregated intrastate effect counts as ''substantial'' for commerce jurisdiction?',
    'Econometric estimation of cross-border spillovers and demand-interdependence for challenged activity classes; comparative doctrine in jurisdictions with quantified thresholds.',
    'A tightened threshold collapses this constraint toward its limited sibling (smaller domain, higher entry costs); a loosened one expands federal reach further and raises effective extraction on subordinated seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantiality_threshold, empirical, 'Threshold indeterminacy in the aggregate-effects standard.').

omega_variable(
    pretext_frequency,
    'How often are aggregate-effects findings genuine economic judgments versus post hoc cover for regulation aimed at noneconomic ends?',
    'Systematic audit of legislative records and findings against independent economic evidence for statutes resting solely on aggregate-effects jurisdiction.',
    'High pretext frequency would push the computed classification toward the snare end — coordination story as cover — and strengthen the case for the limited reading''s non-pretext requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_frequency, empirical, 'Pretext rate in the reading''s operational history (Lopez''s central objection).').

omega_variable(
    firm_net_position,
    'Are large interstate firms net beneficiaries of federal uniformity, or net payers once federal compliance burdens exceed avoided multi-state transaction costs?',
    'Compliance-cost studies comparing single-federal-regime costs against counterfactual fifty-state patchwork costs, sector by sector; lobbying-pattern analysis (industries demanding preemption versus demanding deference).',
    'If firms are net payers, the corporate seat''s directionality rises sharply, weakening the coordination-function half of the tangled-rope reading; if net beneficiaries, current declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_net_position, empirical, 'Net position of the corporate seat is mixed ex ante and sector-dependent.').

omega_variable(
    live_mandate_vs_rent_preservation,
    'Does the arrangement persist because genuinely interstate problems require it, or because federal institutions defend accumulated jurisdiction?',
    'Natural experiments from delegated state autonomy: state marijuana regimes, Medicaid waivers, the Clean Air Act California waiver — compare outcomes and externality management where federal backstop is relaxed.',
    'Comparable state-level performance under waivers indicates a larger inertial/rent component and supports piton-drift monitoring; clear state failure indicates a live mandate sustaining the coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(live_mandate_vs_rent_preservation, empirical, 'Persistence driver: live coordination need versus jurisdictional rent defense.').

omega_variable(
    cs_framing_underdetermination,
    'Is the constraint the doctrinal test itself (an interpretive standard courts apply) or the jurisdictional allocation it produces (federal supremacy over a vast economic domain)?',
    'Conceptual analysis: the test-framing reads as a rule-standard with low intrinsic extraction; the allocation-framing reads as a structural transfer with the asymmetries declared here. Signals guiding the authored choice: the story''s parties, victims, and receipt surface all attach to the allocation, not to the test text.',
    'Under the test-framing the constraint computes nearer rope (standard applied in good faith); under the allocation-framing it computes as the tangled_rope authored here. Framing choice is load-bearing for classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent CS framings of one arrangement with divergent classifications; allocation-framing adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_text__expansive_federal_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_text__expansive_federal_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(comm_tr_t48, commerce_clause_text__expansive_federal_reading, theater_ratio, 48, 0.33).
narrative_ontology:measurement(comm_tr_t64, commerce_clause_text__expansive_federal_reading, theater_ratio, 64, 0.38).
narrative_ontology:measurement(comm_tr_t80, commerce_clause_text__expansive_federal_reading, theater_ratio, 80, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(comm_be_t16, commerce_clause_text__expansive_federal_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comm_be_t32, commerce_clause_text__expansive_federal_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(comm_be_t48, commerce_clause_text__expansive_federal_reading, base_extractiveness, 48, 0.51).
narrative_ontology:measurement(comm_be_t64, commerce_clause_text__expansive_federal_reading, base_extractiveness, 64, 0.59).
narrative_ontology:measurement(comm_be_t80, commerce_clause_text__expansive_federal_reading, base_extractiveness, 80, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comm_su_t16, commerce_clause_text__expansive_federal_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(comm_su_t32, commerce_clause_text__expansive_federal_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement(comm_su_t48, commerce_clause_text__expansive_federal_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement(comm_su_t64, commerce_clause_text__expansive_federal_reading, suppression_requirement, 64, 0.49).
narrative_ontology:measurement(comm_su_t80, commerce_clause_text__expansive_federal_reading, suppression_requirement, 80, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Commerce Clause' into three epsilon-invariant stories: originalist_narrow_reading (historically upstream; smallest domain; near-zero extraction on its own terms), this expansive_federal_reading (largest domain; moderate-high extraction with genuine coordination function), and substantial_effects_limited_reading (middle position; inherits the expansive reading's domain concept while importing entry costs). Each story carries its own beneficiaries, victims, and claimed type; the family is linked bidirectionally through affects_constraints. Upstream/downstream structure: the narrow reading supplies the textual baseline the other two depart from; the expansive reading's dominance created the legitimacy conditions under which the limited reading emerged as corrective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
