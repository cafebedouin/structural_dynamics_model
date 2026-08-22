% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: Security Council Permanent-Five Veto (War-Prevention Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter requires the concurring votes of the
 *   permanent members for substantive Security Council resolutions; the
 *   operative effect is that no binding Council action can pass over any one
 *   of the five. This story instantiates the coordination reading of that
 *   arrangement: the veto is the price that keeps the great powers inside
 *   collective security, guaranteeing each of them against being outvoted
 *   into a military confrontation it rejects. Epsilon is authored for the
 *   standing veto arrangement itself, assessed by this reading's own lights —
 *   never for an alternative decision rule this reading might prefer — and
 *   lands at low-moderate (0.28): mostly the operating cost of the
 *   war-prevention function, with a rising component from vetoes that shield
 *   third-party clients rather than the caster. Under this reading there is
 *   no victim class: the costs of Council paralysis fall diffusely on the
 *   membership as a whole, and the acute cost-bearers in vetoed theaters are
 *   seated as excluded voices rather than declared victims — that is this
 *   reading's own structural claim, fixed here so the file stays
 *   epsilon-invariant. The claim and the metrics are independent authored
 *   facts: the type is claimed as rope from this reading's seat, while the
 *   metrics describe creeping extraction over the interval; where the
 *   engine's per-seat computations diverge from the claim, that divergence is
 *   the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - p5_great_powers: agenda-setters and principal beneficiaries (institutional/constrained) — hold and administer the veto; each collects immunity from compelled confrontation and jointly controls all amendment paths
 *   - non_p5_member_states: beneficiaries (organized/constrained) — receive great-power participation in collective security and freedom from wars authorized over a great power's objection; bear diffuse paralysis costs
 *   - populations_in_vetoed_theaters: excluded (powerless/trapped) — bear the acute costs of blocked action; no seat, no vote, no champion in the chamber
 *   - veto_reform_coalition: excluded (organized/constrained) — restraint-code and transparency initiatives with majority support but no amendment leverage
 *   - international_law_community: analytical observer (moderate/analytical) — tracks usage patterns and doctrinal consequences across the full record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.28).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.55).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "Security Council Permanent-Five Veto (War-Prevention Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

domain_priors:requires_active_enforcement(article_27_veto_power__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e').
narrative_ontology:cs_kernel_codification('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', fixed_text).
narrative_ontology:cs_authority_grounding('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', lineage).
narrative_ontology:cs_interpretation_layer_present('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e').
narrative_ontology:cs_reading_relation('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', foundational, compelled_confrontation_risk_dominates).
narrative_ontology:cs_axiom_status(compelled_confrontation_risk_dominates, holdable).
narrative_ontology:cs_axiom_grounding('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', compelled_confrontation_risk_dominates, empirically_contingent).
narrative_ontology:cs_axiom('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', secondary, unanimity_gate_secures_participation).
narrative_ontology:cs_axiom_status(unanimity_gate_secures_participation, holdable).
narrative_ontology:cs_axiom_grounding('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', unanimity_gate_secures_participation, instrumental).
narrative_ontology:cs_reference_frame('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', great_power_concert_bargain).
narrative_ontology:cs_drift_state('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', contemporary_p5_discord_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a2a7f03-cd64-4d57-a5c6-f5fafe899d1e', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_great_powers).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states hold permanent seats and a negative vote over every substantive Security Council decision. Each uses that vote to guarantee that no Council resolution can order or authorize military action against it or march its forces into a confrontation it rejects. They wrote the rule in 1945, defend it in every reform negotiation, and are the only governments whose ratification any Charter amendment requires. Walking out of the framework would cost them the legitimating cover of collective security, so they operate inside rules they themselves control.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_great_powers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_great_powers, beneficiary).

% The remaining member states vote in the Council but can be overridden on any substantive matter by a single permanent-member negative vote. They receive the continued presence of the great powers inside a legal framework — the alternative having been demonstrated by the League's collapse — and the assurance that no war will be authorized over a great power's objection. They bear Council paralysis whenever the five disagree, and they organize through caucuses and initiatives to condemn or route around individual vetoes, but no voting arithmetic they control can amend the rule.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_member_states, beneficiary,
    organized, biographical, constrained, global).

% People in conflict zones where protective or coercive resolutions were blocked by a permanent-member negative vote — sieges, annexations, and bombardments that continued under the shelter of a cast veto. They hold no vote, occupy no seat, and often have no state willing to spend negotiating capital on their behalf inside the chamber; their recourse is humanitarian channels that operate outside Council procedure entirely.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, populations_in_vetoed_theaters, excluded,
    powerless, immediate, trapped, regional).

% A cross-regional grouping of governments and initiatives — the ACT group's restraint code, the French-Mexican voluntary limitation pledge, the Liechtenstein initiative mandating Assembly debate after every veto. They can convene, publish, and lobby, but every amendment path runs through ratification by the five themselves, so their leverage terminates at the chamber door however wide their support.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, veto_reform_coalition, excluded,
    organized, biographical, constrained, global).

% Scholars, legal advisers, and practitioners who track the veto's usage record, analyze its doctrinal treatment of procedural versus substantive questions, and advise delegations on precedent. They see the full structure across cases and decades; their stake is the coherence of the legal order and the accuracy of the precedent record rather than any flow controlled by the rule.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_law_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, p5_great_powers).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the great-power participation problem in collective security: by making every substantive Council decision subject to each permanent member's concurrence, the rule guarantees that no great power can be outvoted into an enforced confrontation, which is the condition under which all five stayed inside the institution after 1945 instead of repeating the League pattern of walkout and irrelevance.
% TRANSFER_FUNCTION: Moves blocking control over binding collective action from the Council majority to five permanent holders, and moves the corresponding assurance — immunity from compelled confrontation — to those five. The forgone timely action in divided-interest crises is borne by the wider membership and, acutely, by populations in the theaters concerned.
% ABSENT_VOICES: Populations in theaters where action was vetoed have no seat and no vote; the broad membership is locked out of amendment by the ratification rule even where it speaks with near-unanimity through the Assembly; reform coalitions are heard in debate but hold no decision rights anywhere in the chain that could change the text.
% DISAPPEARANCE_RATIONALE: Overnight removal of the unanimity gate forces an immediate choice on the five: defy binding resolutions they reject (compliance crisis, and the credible threat of enforcement against nuclear states collapses the Council's authority) or accept compulsion (direct confrontation risk). Either branch dismantles the Chapter VII architecture; the realistic reconstruction is some renewed great-power concert outside the Charter or a reversion to sphere-of-influence politics — the arrangements the 1945 design was built to displace.
% FOUNDING_PROBLEM: In 1945 the founders confronted the League's demonstrated failure mode: an institution that could bind or exclude the great powers lost them — aggressors walked out, sanctions failed, and the organization watched the war it existed to prevent. The founding problem was designing a collective-security body the great powers would join and remain inside, under conditions where the atomic age made their direct confrontation catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration outside the beneficiary set is partial and should be stated plainly. Independent support comes from the League failure record itself, from diplomatic practice of non-permanent members — governments that harshly criticize individual vetoes nonetheless build policy around great-power participation and pursue restraint codes rather than abolition — and from international-relations scholarship across rival theoretical traditions, which disputes the veto's justice but broadly concedes the participation dynamic. The strongest attestations of the founding problem, however, still come from the five themselves, and that asymmetry is itself signal: the genealogy is corroborated at the level of the participation problem, contested at the level of what the privilege is currently used for.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.28: under this reading most veto exercises purchase the core function (no great power is compelled), but the client-shielding share has grown since the 1970s and dominates recent usage, so the extraction component trends upward across the series. Suppression 0.55: the veto actively nullifies binding Council action whenever the five diverge — a real suppression of the collective-security channel — while leaving extra-Council alternatives (Assembly session, ad hoc coalitions, unilateral action) open, which caps it well below coercive-system levels. Theater_ratio 0.12: the mechanism is operative, not performed; the slight rise reflects growing rhetorical framing of each veto as responsibility-exercise rather than its blocking work. Accessibility_collapse 0.45: once the arrangement is understood, formal alternatives collapse (any amendment requires the five's own ratification), but workable partial substitutes persist — Assembly mechanisms, voluntary restraint codes, routing around the Council — each with known failure modes inherited from the League record. Resistance 0.55: sustained and institutionalized (Uniting for Peace 1950, the ACT code of conduct, the Franco-Mexican pledge, the Liechtenstein veto initiative) yet ineffective at the textual level for eight decades. The suppression series is deliberately cyclical rather than monotonic — Cold War peak, detente decline, post-Cold-War trough in the 1990s when the five cooperated, renewed climb through the Ukraine and Middle East crises — tracking the great-power discord cycle; nine points capture one full cycle. Whether the oscillation itself is an extraction mechanism (intermittent blocking training the membership into dependence on P5 goodwill) or a side effect of external geopolitics is flagged in the client_shielding_boundary omega. All three tracked metrics are authored on one shared nine-point grid so no end-state value is silently substituted into earlier periods.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the permanent-member seat the arrangement is self-defense insurance plus the service of presence: we stay at the table, and the table can never be turned into a weapon against us. From the non-permanent-member seat it is a diffuse tax — paralysis paid by everyone — funding a public good (great-power restraint) whose incidence is invisible until a crisis exposes it. From the excluded seats — vetoed-theater populations and the reform coalition — the same structure presents as rule by five, with costs concentrated on those with the least voice. The engine derives these per-seat classifications from the declared positions and exits; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared beneficiary groups derive low directionality: the permanent members sit nearest the beneficiary pole (designers, administrators, and collectors of the immunity guarantee, with constrained exit that binds them to the system they run), and the broader membership sits near-symmetric-but-net-positive under this reading — real paralysis costs, outweighed by the participation dividend the reading asserts. No victims are declared because this reading holds there is no victim class; the acute cost-bearers are seated as excluded, and per the R3 ruling their authored absence feeds commentary, never a directionality correction. Suppression enters the engine's computation as a raw structural property, unscaled; only extractiveness is scaled, modestly amplified here by the global spatial scope that makes verification of every member's position harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the great powers inside a common institution in the nuclear age — remains live, so there is no mandate atrophy to resolve: function and persistence coincide under this reading, and the R5 mismatch consumer reads status=live against verdict=world_rearranges with no zombie flag available. The live risk to this reading is temporal, not logical: the extraction series rises monotonically while the suppression series cycles. If the client-shielding share continues to grow until blocking-for-others dominates blocking-for-self, the arrangement's persistence decouples from its founding function while the founding problem technically remains live — the precise condition the amendment_lock_persistence omega is designed to detect, and the point at which this file's rope claim would need revisiting on the evidence rather than on rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel article_27_veto_power — the coordination_reading, which holds the veto is justified by war-prevention necessity. Sibling readings (separate constraint files) are the oligopoly_reading (veto as entrenched authority rent extracted through Charter immutability) and the sovereignty_reading (veto as Westphalian consent principle applied to great powers). What would adopting a sibling change structurally?',
    'Comparative classification across the three reading files over the identical referent (the standing veto arrangement). The disagreement is located in what explains the veto''s persistence: functional necessity (this reading), rent entrenchment (oligopoly), or consent-based legitimacy (sovereignty).',
    'If the oligopoly reading is structurally correct, epsilon for the same arrangement is far higher and victim classes exist (blocked-reform membership, vetoed-theater populations), pushing classification toward extraction-dominated types. If the sovereignty reading is correct, the veto is a principled limit whose costs are constitutive rather than incidental. This file''s low-moderate epsilon is valid only under this reading''s own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: this story instantiates one of three live readings of the Article 27 veto kernel.').

omega_variable(
    war_prevention_counterfactual,
    'Would great-power war actually be more probable without the unanimity gate — that is, is the coordination function real, independently of nuclear deterrence as a rival explanation for the post-1945 great-power peace?',
    'Historical and archival comparison: League of Nations exclusion/walkout dynamics versus post-1945 crisis episodes where the gate plausibly lowered escalation pressure (confrontations resolved by Council paralysis rather than ultimata), controlling for the nuclear deterrent variable across dyads and periods.',
    'If nuclear deterrence rather than the veto explains great-power restraint, the coordination justification collapses, the measured extraction becomes uncompensated cost, and the classification shifts sharply upward in extractiveness. If the gate has independent causal weight, the current profile stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(war_prevention_counterfactual, empirical, 'Whether the veto''s war-prevention function is causally real or confounded by nuclear deterrence.').

omega_variable(
    client_shielding_boundary,
    'What share of veto exercises protect the vetoing power itself from compelled confrontation (the coordination function) versus shield third-party clients or proxy interests (a use the coordination rationale does not cover)?',
    'Code the full veto record by whether the casting power faced any realistic prospect of binding action against itself in the matter, versus shielding an allied government or annexation/proxy asset; publish the distribution by era.',
    'A rising client-shielding share means a growing fraction of the arrangement''s operation extracts from the broader membership and vetoed-theater populations without purchasing war prevention — the signal that function and persistence are decoupling and that a hybrid classification would become the honest terminal state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(client_shielding_boundary, empirical, 'Boundary between self-protection vetoes (function) and client-shielding vetoes (cost without function).').

omega_variable(
    amendment_lock_persistence,
    'Does the veto persist because it remains necessary (functional persistence) or because Article 108''s ratification requirement makes removal impossible regardless of function (lock-in persistence)?',
    'Behavioral test: observe P5 responses to binding-form restraint proposals (code-of-conduct pledges with reporting obligations, automatic Assembly referral). Willingness to accept enforceable self-limitation indicates the privilege is held for its function; refusal across all forms indicates the privilege is held for itself.',
    'If lock-in rather than necessity explains persistence, the arrangement is held aloft by constitutional impossibility of exit rather than ongoing coordination value, and its honest steady-state classification is inertial rather than coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_lock_persistence, conceptual, 'Functional persistence versus constitutional lock-in as the explanation for the veto''s survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__coordination_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__coordination_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__coordination_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__coordination_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__coordination_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__coordination_reading, theater_ratio, 70, 0.11).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__coordination_reading, theater_ratio, 80, 0.12).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__coordination_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__coordination_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__coordination_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__coordination_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__coordination_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__coordination_reading, base_extractiveness, 70, 0.26).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__coordination_reading, base_extractiveness, 80, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__coordination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__coordination_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__coordination_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__coordination_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__coordination_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(arti_su_t50, article_27_veto_power__coordination_reading, suppression_requirement, 50, 0.32).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__coordination_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(arti_su_t70, article_27_veto_power__coordination_reading, suppression_requirement, 70, 0.5).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__coordination_reading, suppression_requirement, 80, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (Article 27 veto power), three readings emitted as separate stories. All three share the same epsilon referent — the standing veto arrangement itself — but author reading-indexed epsilon over it: this coordination_reading authors low-moderate epsilon (mostly the operating cost of keeping great powers inside collective security, with a rising client-shielding component); the oligopoly_reading authors high epsilon with explicit victim classes; the sovereignty_reading authors the arrangement as a consent-based limit whose costs are principled. Family links run through network.affects_constraints in all three files; this reading is the upstream founding-era justification from which the other two dissent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
