% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Credential Requirements as Rent-Seeking Labor Supply Restriction
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This constraint instantiates the RENT-SEEKING SUPPRESSION reading of the
 *   licensing statute kernel. Statutory credential requirements are presented
 *   as consumer protection mechanisms (the public-safety reading, a sibling
 *   story in
 *   constraint_licensing_statute_mandate__public_safety_coordination) and as
 *   tiered-access filters that sort entrants by prior resource (the
 *   graduated-access reading, in
 *   constraint_licensing_statute_mandate__graduated_access_filter). This
 *   reading treats the observed credential structure as primarily extractive:
 *   incumbents benefit from restricted labor supply; entry-seekers and
 *   consumers bear costs. The statute is legitimate cover for what functions
 *   as a cartel. The measured metrics (extractiveness 0.81, theater ratio
 *   0.68) reflect the asymmetry: genuine coordination function is minimal;
 *   performative consumer-protection rhetoric is substantial. The
 *   claim/metric alignment is deliberate: this reading claims SNARE, and the
 *   metrics describe snare operation—income extraction via supply
 *   restriction, defended by enforcement machinery (prosecution of unlicensed
 *   practice), sold under coordination (consumer safety) narrative.
 *
 * KEY AGENTS:
 *   - incumbent_licensed_practitioners: organized agenda-setters benefiting from supply scarcity; power=organized, exit=arbitrage (credentials protect their past investment)
 *   - credential_entry_seekers: powerless targets bearing education costs and restricted-supply wage suppression; power=powerless, exit=identity_locked (career identity commitment raises exit cost)
 *   - consumers_via_higher_prices: powerless targets bearing diffuse cost from elevated prices; power=powerless, exit=constrained (switching to unlicensed substitutes is risky and often legally barred)
 *   - professional_licensing_board: institutional agenda-setter administering the requirement; structurally captured by incumbent practitioners; power=institutional, time_horizon=generational
 *   - enforcement_machinery: non-agent entity (prosecution, civil penalties) that maintains the supply restriction by suppressing unlicensed practice
 *   - alternate_credential_providers: organized excluded parties who could offer cheaper entry but are legally barred; power=moderate, exit=trapped (cannot compete with statutory credential)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.81).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.77).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.81).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Credential Requirements as Rent-Seeking Labor Supply Restriction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '95e806f4-138b-4188-8b59-9aea7b8b35ce').
narrative_ontology:cs_kernel_codification('95e806f4-138b-4188-8b59-9aea7b8b35ce', formalized).
narrative_ontology:cs_authority_grounding('95e806f4-138b-4188-8b59-9aea7b8b35ce', extraction).
narrative_ontology:cs_interpretation_layer_present('95e806f4-138b-4188-8b59-9aea7b8b35ce').
narrative_ontology:cs_reading_relation('95e806f4-138b-4188-8b59-9aea7b8b35ce', licensing_statute_mandate__public_safety_coordination, forecloses).
narrative_ontology:cs_reading_relation('95e806f4-138b-4188-8b59-9aea7b8b35ce', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('95e806f4-138b-4188-8b59-9aea7b8b35ce', foundational, credential_scope_exceeds_competence_requirement).
narrative_ontology:cs_axiom_status(credential_scope_exceeds_competence_requirement, holdable).
narrative_ontology:cs_axiom_grounding('95e806f4-138b-4188-8b59-9aea7b8b35ce', credential_scope_exceeds_competence_requirement, empirically_contingent).
narrative_ontology:cs_axiom('95e806f4-138b-4188-8b59-9aea7b8b35ce', foundational, incumbent_practitioners_drive_credential_strictness).
narrative_ontology:cs_axiom_status(incumbent_practitioners_drive_credential_strictness, holdable).
narrative_ontology:cs_axiom_grounding('95e806f4-138b-4188-8b59-9aea7b8b35ce', incumbent_practitioners_drive_credential_strictness, instrumental).
narrative_ontology:cs_reference_frame('95e806f4-138b-4188-8b59-9aea7b8b35ce', supply_competitive_baseline).
narrative_ontology:cs_drift_state('95e806f4-138b-4188-8b59-9aea7b8b35ce', contemporary_regulatory_capture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('95e806f4-138b-4188-8b59-9aea7b8b35ce', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, credential_entry_seekers).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_via_higher_prices).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, regulatory_capture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Existing practitioners have completed credential requirements and hold licenses. They benefit from restricted labor supply that suppresses wage competition and allows above-marginal-cost pricing. They actively shape credential scope and difficulty through professional boards, licensing committees, and legislated renewal requirements. Their exit from the arrangement is not exit at all: holding a credential creates vested interest in its scarcity and gatekeeping value.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners, agenda_setter).

% Individuals seeking to enter the regulated profession face statutory education requirements, examination fees, apprenticeship periods, and continuing education costs. The credential requirement gates economic access to the profession. Their alternatives are entering an unregulated adjacent field (which may not be the field they trained for) or bearing the full cost of the gatekeeping mechanism. Many are identity-committed to the profession (career aspiration, self-concept, family expectation) making exit psychologically costly even beyond economic cost.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, credential_entry_seekers, payer,
    powerless, biographical, identity_locked, national).

% Pay higher service prices than would clear under competitive labor supply. The credential restriction reduces the number of suppliers and suppresses wage pressure, allowing incumbents to price above marginal cost. Consumers cannot easily verify whether a higher price reflects genuine safety concerns or artificial scarcity rent. They bear the cost diffusely (embedded in prices for haircuts, medical exams, legal advice, electrical work) making the extraction difficult to organize against.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_via_higher_prices, payer,
    powerless, immediate, constrained, national).

% State or professional body that administers the credential requirement. Board membership is typically dominated by incumbent practitioners (self-regulation). The board sets examination standards, renewal requirements, and scope of practice. Its mandate frames credential restrictions as consumer protection; its structural incentives align with maintaining restriction severity. Changing the board's behavior would require external legislative override.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_board, agenda_setter,
    institutional, generational, trapped, national).

% The statutory framework that delegates credential authority to professional bodies. Legislatures periodically receive complaints about credential restriction's effect on supply and pricing but face organized incumbent opposition to any loosening. Legislative action to deregulate or open alternative pathways is politically expensive and rarely happens absent crisis (pandemic shortages, wage shocks, public pressure campaigns).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, legislative_authority, observer,
    institutional, generational, analytical, national).

% Non-traditional education providers (online schools, employer training programs, community colleges, apprenticeship networks) could offer faster, cheaper entry to competence but are legally barred from certifying completion as equivalence to statutory credentials. They are excluded from the arrangement not by market dynamics but by legal restriction. Their exclusion is enforced through prosecution of unlicensed practice.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, alternate_credential_providers, excluded,
    moderate, biographical, trapped, national).

% Inspectorates, prosecution bodies, and civil penalties that criminalize unlicensed practice. The enforcement system exists to suppress alternatives and maintain the credential requirement's binding force. Prosecution of unlicensed practitioners is the operational mechanism that keeps the artificial scarcity intact.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, enforcement_machinery, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(licensing_statute_mandate__rent_seeking_suppression, enforcement_machinery).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine to this reading: credential standardization could serve a real coordination function (common baseline for consumer safety, labor market signaling), but the rent-seeking reading attributes the observed credential structure to its restrictive effect on supply, not to coordination necessity. Any residual coordination function is incidental cover for extraction.
% TRANSFER_FUNCTION: Moves economic surplus from entrants (who bear credential costs and accept restricted-supply wage suppression) and consumers (who pay above-competitive prices) to incumbents (whose wages stay elevated and whose human capital is protected from dilution). The transfer is mediated through restricted labor supply maintained by statutory gatekeeping.
% ABSENT_VOICES: Entrants who never begin the education pathway because it is too costly, consumers who switch to unregulated substitute services, non-traditional credential providers legally barred from operating, and economic analysts who calculate the welfare cost of artificial scarcity. These voices are systematically excluded from professional licensing boards and from legislative credential reform debates.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished overnight, entry to the profession would accelerate, incumbent wages would face downward pressure, and consumer prices would eventually decline as supply expanded. Incumbent practitioners would experience income loss; entrants and consumers would gain. The profession would reorganize around reputation signaling and employer screening rather than state-mandated gatekeeping. The beneficiaries have structural incentive to prevent this rearrangement.
% FOUNDING_PROBLEM: Early regulation claimed to solve: incompetent practitioners harming consumers due to information asymmetry. Consumer cannot easily assess competence before purchasing service; statutory credential was presented as solution—a government-backed signal substituting for market reputation mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners attest the founding problem remains urgent and justify credential strictness as consumer protection. Entry-seekers and economists attest the founding problem is largely solved (reputation mechanisms, employer training, consumer reviews online exist), and credential requirements now exceed what consumer protection requires. Empirical studies of credential scope vs. actual consumer harm show weak correlation in many fields; legislative testimony from states that liberalized credentials reports continued consumer safety at lower cost. No single authoritative external source; the contest is live.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the measured constraint—statutory credential requirements—directly restricts labor supply, which allows incumbents to price above marginal cost and earn above-competitive wages. The restriction is artificial (not justified by competence gaps proportional to credential scope in this reading). Suppression is substantial (0.77) because the constraint maintains its force through prosecution of unlicensed practice, educational gatekeeping (expensive credential programs control entry), and identity-lock (entrants are emotionally committed to the profession they are being charged to access). Theater ratio is high (0.68) indicating that a significant fraction of credential administration is performative: continuing education requirements, scope restrictions, and renewal hurdles serve little consumer-protection function in the rent-seeking reading but maintain the appearance of rigorous oversight. This reading treats the founding-problem claim (consumer safety) as the narrative cover; the operational objective is supply restriction. The measurement trajectory shows extractiveness and suppression rising moderately over the interval (0.68→0.81, 0.71→0.77) as credential scope expands and as incumbent practitioners tighten requirements in response to the threat of deregulation or alternative credentials. Theater ratio plateaus (0.58→0.68) as the ratio of performative to functional activity stabilizes—the constraint has matured into its equilibrium form.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (professional_licensing_board, staffed by incumbents) and the beneficiary (incumbent_licensed_practitioners) experience this constraint as legitimate coordination solving a real consumer-harm problem; they author a narrative of minimal necessary gatekeeping. The victims (entry-seekers, consumers) experience it as coercive artificial scarcity they bear costs to maintain. The engine computes this divergence from structural data: incumbent practitioners derive directionality ~0.1 (beneficiary end); entry-seekers derive ~0.95 (target end); consumers derive ~0.80 (target end with some constrained-exit mobility). The same institutional arrangement computes as 'legitimate coordination with tight-but-defensible gatekeeping' for the first seat and 'cartel enforcement' for the other seats. The metrics do not reconcile this gap—they document it. A reading from the professional board's seat would author lower extractiveness and theater ratios; this reading treats the board-seat framing as capture (a seat with structural incentive to justify the arrangement it administers).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners: low directionality (~0.15), strong beneficiary. They set the agenda, control the professional board, and derive economic surplus from restricted supply. Their time horizon is generational (professional careers), their exit is arbitrage-grade (they can shift to unlicensed adjacent work but the credential value anchors them to the licensed space). Directionally they are far-end beneficiaries—the constraint's entire structure exists to protect their position. Entry-seekers: high directionality (~0.92), near-total targets. They are powerless, identity-locked (career commitment), face trapped or at-best constrained exit, and bear direct costs (education expenses, wage suppression). Directionally they are near-total targets. Consumers: high directionality (~0.80), substantial targets. They are powerless, face constrained exit (switching to unlicensed care is legally risky, informationally uncertain, or unavailable), and bear diffuse elevated-price cost. Directionally they are substantial targets, less extreme than entry-seekers only because the cost is diffuse and they have some weak exit via substitutes. Professional licensing board: moderate directionality (~0.55), roughly symmetric because the board is structurally part-beneficiary (its power derives from credential authority) and part-enforcer-of-victim-terms (it administers the costs). Captured by incumbents, so functionally aligned with the beneficiary seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The rent-seeking reading dissolves any mandatrophy ambiguity: the founding problem (consumer harm from incompetence) is either solved by alternative mechanisms (the entry-seekers and economists reading) or overstated by the incumbent practitioners (the conspiracy reading). Either way, the mandate has outlived its primary justification; what remains is supply-restriction maintenance, a zombie function. The theater ratio (0.68) indicates that the bulk of credential administrative activity is now performative rather than functional—scope expansions, renewal hurdles, and continuing education requirements serve the supply-restriction mission more than any residual consumer-protection mission. The constraint has completed the transition from 'justified gatekeeping' to 'mandatrophy defense'—the mandate persists as self-perpetuating administrative theater, not as active coordination. The base_properties.mandatrophy_resolved flag is NOT set because the founding problem's obsolescence is contested (the incumbent reading insists it is live); the omega documents the dispute. But the measurement trajectory and theater ratio combine to mark this as a high-confidence mandatrophy candidate from the external analytical seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the original consumer-harm coordination problem that justified credential requirements been substantially solved by non-regulatory mechanisms (reputation, online reviews, employer screening, insurance), making current credential strictness obsolete rent-extraction rather than protection?',
    'Empirical: comparative consumer harm rates between regulated and alternative-certified practitioners in the same field. Econometric: regression of credential scope vs. consumer complaint rates, controlling for market maturity and alternative reputation infrastructure.',
    'If founding problem is substantially solved, the constraint reclassifies from coordination-with-extraction (tangled rope candidate) to pure extraction (snare). The theater-ratio rise documents the transition: real coordination function shrinking, performative credential maintenance growing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether statutory credentials still solve their founding consumer-protection problem or now primarily restrict supply.').

omega_variable(
    identity_lock_vs_economic_constraint,
    'For credential entry-seekers, how much of the suppression (0.77) is structural economic barrier vs. internalized identity commitment to the profession, and would suppression persist if economic barriers were removed?',
    'Natural experiment: jurisdictions that eliminate credential requirements; track whether exit-seekers (those who would have entered under barrier removal) perceive lingering psychological barriers after legal removal. Post-exit trajectory: do persons denied entry under the old regime remain profession-aspiring or shift identity/goals?',
    'If suppression is substantially internalized, the constraint''s effective extraction is higher than the measured structural barrier suggests—the target carries suppression with them after exit. The measured 0.77 understates the constraint''s grip. If structural only, the 0.77 accurately captures the coercive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_economic_constraint, empirical, 'Whether credential suppression is structural barrier or internalized identity commitment.').

omega_variable(
    kernel_reading_contest,
    'Is this constraint a snare (rent-seeking reading) or a tangled rope (public safety reading), or a class-stratification mechanism (graduated access reading)? The statute''s legitimacy claim rests on public safety; the observational evidence shows restricted supply and incumbent benefit; but causation and primary function remain contested across these three sibling readings.',
    'Structural: compare credential requirements to evidence-based competence standards (does the requirement track actual skill gaps?). Historical: examine legislative intent in credential creation debates (explicit intent to restrict supply vs. protect consumers). Comparative: examine jurisdictions that dropped credentials and assess public safety outcomes.',
    'The three readings emit different ε values (public_safety=low extraction justified by coordination; graduated_access=moderate extraction tied to class gatekeeping; rent_seeking_suppression=high extraction from artificial scarcity). The engine computes seat-level types from structural data; this omega documents the kernel ambiguity that permits three readings to claim the same statute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading of the licensing statute kernel is the correct structural interpretation.').

omega_variable(
    professional_board_capture_depth,
    'To what extent is the professional licensing board captured by incumbent practitioners vs. operating as an autonomous regulator? Is board-member dominance by incumbents the cause of oversupply restriction, or merely correlated with it?',
    'Institutional analysis: examine board composition rules, term lengths, appointment authority, and conflict-of-interest disclosure. Behavioral: compare credential-tightening rates in jurisdictions where boards are incumbent-dominated vs. mixed-composition. Legislative intervention: track whether external (non-board) overrides of credential requirements succeed.',
    'Capture depth determines whether the snare''s primary enforcer (professional_licensing_board) is truly agenda-setting (captured) or administratively constrained. If captured, remediation requires restructuring board composition; if autonomous, remediation requires legislative mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_board_capture_depth, empirical, 'Whether professional licensing boards are captured by incumbents or independent administrators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.58).
narrative_ontology:measurement_basis(lice_tr_t0, projected).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 5, 0.61).
narrative_ontology:measurement_basis(lice_tr_t5, observed).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.63).
narrative_ontology:measurement_basis(lice_tr_t10, observed).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 15, 0.65).
narrative_ontology:measurement_basis(lice_tr_t15, observed).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.66).
narrative_ontology:measurement_basis(lice_tr_t20, observed).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 25, 0.67).
narrative_ontology:measurement_basis(lice_tr_t25, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.68).
narrative_ontology:measurement_basis(lice_tr_t30, observed).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(lice_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(lice_be_t0, projected).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(lice_be_t5, observed).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(lice_be_t10, observed).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(lice_be_t15, observed).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(lice_be_t20, observed).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(lice_be_t25, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(lice_be_t30, observed).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(lice_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(lice_su_t0, projected).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 5, 0.73).
narrative_ontology:measurement_basis(lice_su_t5, observed).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(lice_su_t10, observed).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(lice_su_t15, observed).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(lice_su_t20, observed).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(lice_su_t25, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.77).
narrative_ontology:measurement_basis(lice_su_t30, observed).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.77).
narrative_ontology:measurement_basis(lice_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.08).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the licensing_statute_mandate kernel. All three stories share the same statutory structure but disagree on primary function (supply restriction vs. consumer protection vs. class gatekeeping) and primary beneficiary (incumbents vs. consumers vs. wealthy entrants). The constraint family is linked via network.affects_constraints pointing to the other readings. The ε values differ substantially across readings (public_safety_coordination ≈ 0.15, graduated_access_filter ≈ 0.52, rent_seeking_suppression ≈ 0.81) reflecting structurally different causal claims about the statute's actual operation, not different observables measuring one constraint. Each reading is a standalone constraint story with complete structural data, not a perspective on shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
