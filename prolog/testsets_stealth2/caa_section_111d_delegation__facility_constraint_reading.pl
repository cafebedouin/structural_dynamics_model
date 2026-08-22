% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Clean Air Act Section 111(d) 'Best System' — Facility-Constraint Reading
 *   domain: administrative law / environmental regulation / constitutional interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to determine the 'best
 *   system of emission reduction' adequately demonstrated for existing
 *   sources, taking cost, nonair impacts, and energy requirements into
 *   account. This story authors the facility-constraint arrangement: under
 *   it, the 'best system' comprises only measures a regulated source can
 *   implement at the plant — heat-rate improvements, carbon capture and
 *   storage, and like source-level controls — so EPA's existing-source
 *   authority stops at the fenceline. The arrangement was fixed as governing
 *   doctrine in 2022 and now frames every existing-source rulemaking: EPA may
 *   require what a plant can do, not what the generation mix must become. Its
 *   operation protects the existing coal fleet from federally compelled
 *   retirement, preserves state authority over the generation mix inside
 *   state borders, and places a durable ceiling on the federal regulatory
 *   instrument available to climate-policy seekers, who bear the gap between
 *   the decarbonization pace they seek and the pace source-level tools
 *   deliver. Assumptions stated: the interval runs 2015–2025, from the
 *   rulemaking that made the delegation's scope consequential to the present
 *   consolidation of this reading; metric values are authored with the ε
 *   referent fixed to this arrangement as it operates, never to any endorsed
 *   alternative arrangement.
 *
 * KEY AGENTS:
 *   - existing_coal_fleet_owners: primary beneficiary (organized/constrained) — keeps its plants out of federally compelled retirement; sunk capital retains value
 *   - coal_producing_states: beneficiary and co-agenda-setter (institutional/identity_locked) — obtained the reading by litigation, defend it in every rulemaking; mix authority preserved
 *   - supreme_court_majority: agenda_setter (institutional/constrained) — authored the reading and polices its boundary in subsequent review
 *   - climate_advocacy_coalitions: primary target (organized/constrained) — preferred federal instrument foreclosed; bear the pace gap
 *   - communities_bearing_climate_harms: target (powerless/trapped) — diffuse costs of slowed retirement; no procedural seat
 *   - renewable_energy_developers: secondary target (organized/mobile) — one federal demand driver removed; mobile exit into other markets
 *   - epa: constrained administrator (institutional/constrained) — drafts every existing-source rule inside the fenceline limit
 *   - congress: formal beneficiary (institutional/mobile) — holds the reserved power; has not exercised it
 *   - future_generations: excluded voice (powerless/trapped) — inherit the cumulative-emissions outcome; no seat or standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.6).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.55).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Clean Air Act Section 111(d) 'Best System' — Facility-Constraint Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative law / environmental regulation / constitutional interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '9fb056e4-e792-4b7f-b5be-6ed4da50248a').
narrative_ontology:cs_kernel_codification('9fb056e4-e792-4b7f-b5be-6ed4da50248a', fixed_text).
narrative_ontology:cs_authority_grounding('9fb056e4-e792-4b7f-b5be-6ed4da50248a', lineage).
narrative_ontology:cs_interpretation_layer_present('9fb056e4-e792-4b7f-b5be-6ed4da50248a').
narrative_ontology:cs_reading_relation('9fb056e4-e792-4b7f-b5be-6ed4da50248a', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('9fb056e4-e792-4b7f-b5be-6ed4da50248a', foundational, best_system_limited_to_source_applicable_measures).
narrative_ontology:cs_axiom_status(best_system_limited_to_source_applicable_measures, holdable).
narrative_ontology:cs_axiom_grounding('9fb056e4-e792-4b7f-b5be-6ed4da50248a', best_system_limited_to_source_applicable_measures, conventional).
narrative_ontology:cs_axiom('9fb056e4-e792-4b7f-b5be-6ed4da50248a', secondary, clear_congressional_authorization_for_major_questions).
narrative_ontology:cs_axiom_status(clear_congressional_authorization_for_major_questions, holdable).
narrative_ontology:cs_axiom_grounding('9fb056e4-e792-4b7f-b5be-6ed4da50248a', clear_congressional_authorization_for_major_questions, conventional).
narrative_ontology:cs_reference_frame('9fb056e4-e792-4b7f-b5be-6ed4da50248a', source_specific_cooperative_federalism_regime).
narrative_ontology:cs_drift_state('9fb056e4-e792-4b7f-b5be-6ed4da50248a', post_west_virginia_contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9fb056e4-e792-4b7f-b5be-6ed4da50248a', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, existing_coal_fleet_owners).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_producing_states).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, congress).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_coalitions).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, communities_bearing_climate_harms).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, state_autonomy_over_generation_mix).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, congressional_primacy_on_economy_wide_energy_decisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held in 2022 that the Clean Air Act does not clearly authorize EPA to shift electricity generation across the grid as a means of reducing existing-source emissions, and that decisions of that economic and political scale belong to Congress. Its opinion is the operative statement of the fenceline limit; later benches apply it when EPA rulemakings test the boundary. The Court can revisit its own holding, but stare decisis and the institutional cost of reversing a fresh major ruling make that a heavy step.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, supreme_court_majority, agenda_setter,
    institutional, generational, constrained, national).

% Operate the existing coal-fired generating fleet. The limit keeps federal existing-source standards anchored to measures each plant can install — efficiency upgrades, carbon capture — so no rule can compel their plants to retire or their output to be displaced. Their plants run longer in the dispatch stack and the capital sunk in them retains value. Leaving the fleet means writing off those assets, so they stay and defend the limit through trade associations and litigation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, existing_coal_fleet_owners, beneficiary,
    organized, biographical, constrained, national).

% States whose budgets, employment, and political identity are built around coal production. They initiated the litigation that produced the reading and intervene in every subsequent rulemaking to defend it. The limit preserves their authority over the generation mix inside their borders — they decide which plants retire and when, not EPA. Their fiscal exposure to coal's decline gives them a standing institutional interest in the limit's survival, and their political identity is fused with the industry's fate.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_producing_states, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_producing_states, agenda_setter).

% Organizations and legal campaigns pursuing rapid economy-wide decarbonization. The limit forecloses their preferred federal instrument: a generation-shifting rule for existing sources. They still litigate over where the fenceline boundary sits, press Congress, and work through state law, but a ceiling on federal existing-source authority stands over every route. They bear the gap between the decarbonization pace they seek and the pace the remaining tools deliver.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_coalitions, payer,
    organized, generational, constrained, national).

% Places and populations already living with flood, heat, wildfire, and storm losses that scale with cumulative emissions. They hold no procedural seat in the interpretive process; they appear only through proxy litigants and amicus filings. Their exposure continues for as long as the protected fleet runs longer than an unconstrained rule would have allowed, and they cannot exit the harm by changing jurisdictions.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, communities_bearing_climate_harms, payer,
    powerless, generational, trapped, global).

% Build wind, solar, storage, and transmission capacity. The limit removes one federal demand driver — a rule that would have pushed coal off the grid and pulled their output into its place. They continue to grow on state mandates, tax credits, and falling costs, so they can build for other customers and other markets; what they lose is a specific federal pathway, not their industry.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    organized, biographical, mobile, national).

% Administers Section 111(d) inside the limit: it sets existing-source standards based on what a plant can apply at the facility and designs every rule to fit that frame, with its proposals reviewed against the boundary. It cannot exit the role — the statute assigns it — and the limit caps precisely the discretion it administers.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter).

% Holds the lawmaking power the reading reserves: economy-wide energy decisions require its clear authorization. It has not exercised that power — no comprehensive climate statute has passed. It collects the preserved prerogative while bearing few of the limit's day-to-day costs; any member can move to legislate, though Senate procedure and coal-state members make passage rare.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, congress, beneficiary,
    institutional, biographical, mobile, national).

% People not yet born who will inherit the cumulative emissions that the pace of coal retirement determines. They have no seat, no standing, and no representative in the interpretive process; they act only through the advocacy of others. Their stake in the arrangement's pace effects is the largest and their voice the most absent.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, existing_coal_fleet_owners).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates authority over the electricity system: EPA sets technology-based standards for individual sources; Congress decides economy-wide questions such as the generation mix; states retain control over the plants and portfolios inside their borders. It also gives existing-source regulation an administrable form — a standard tied to what a plant can do is measurable, enforceable, and reviewable in a way a grid-wide redesign directive is not.
% TRANSFER_FUNCTION: Moves regulatory discretion over the pace of coal retirement from EPA to Congress (formally) and to incumbent coal interests and coal-producing states (operationally). It moves avoided retirement and compliance costs to fleet owners, and moves the deferred costs of cumulative emissions to the public and to future populations.
% ABSENT_VOICES: Communities bearing climate harms and future generations would object to the limit's pace effects and are absent — they hold no procedural seat and appear only through amici and proxy litigants. Public-health and climate-science voices entered the record at the margins. The litigating coalition that produced the reading was states and industry; no adversely affected population was a party.
% DISAPPEARANCE_RATIONALE: If the limit vanished overnight, EPA could issue a generation-shifting rule under Section 111(d), coal retirement schedules would accelerate, state authority over the generation mix would narrow, and the coal fleet's remaining asset value would collapse — the electricity sector, the coal regions dependent on it, and the federal-state allocation of energy authority would all reorganize.
% FOUNDING_PROBLEM: An agency claimed the power to restructure the national electricity system — shifting generation away from coal — on a reading of an ambiguous statutory phrase, without clear congressional authorization for a decision of that economic and political scale.
% FOUNDING_PROBLEM_CORROBORATION: Administrative-law scholars who reject the Court's holding nonetheless corroborate that the underlying delegation-and-accountability problem is real and unresolved — Congress has not enacted the comprehensive scheme the reading says it must, and the statutory ambiguity was conceded across the litigation by parties on both sides. No source outside the beneficiary set corroborates the coal-protection framing of the problem; that side of the genealogy rests entirely on the parties who gain from the limit.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the reading's ceiling is a real transfer of regulatory outcome — the decarbonization pace reachable through Section 111(d) is set by what plants can install rather than by what the grid requires, and the difference accrues as extended operating life for the fleet the limit protects. It is not higher because the ceiling binds one instrument among several: state mandates, federal tax credits, new-source standards under Section 111(b), and market cost curves still move the generation mix, and EPA retains genuine if narrowed standard-setting power at the source. Suppression 0.55: the limit holds through judicial enforcement — rules that cross the line are vacated and agencies draft to the line — but it suppresses one regulatory pathway, not the field of climate action; legislators, states, and litigants retain workable alternatives. Theater 0.28: part of the arrangement's stated rationale is performative — the decision the reading reserves to Congress goes unexercised, so the accountability framing currently protects outcomes without enabling decisions — but the interpretive limit also does real blocking work, and a source-level standard is genuinely more administrable and reviewable than a grid-wide one. Accessibility collapse 0.42: once the limit is understood, the generation-shifting pathway collapses for federal existing-source regulation, but the surrounding alternative space (state action, congressional legislation, other statutory titles, market substitution) remains open, so alternatives do not collapse wholesale. Resistance 0.62: the reading meets sustained resistance — boundary litigation over each subsequent rule, scholarly critique of the clear-statement method, and repeated legislative attempts to override it — which keeps the fenceline boundary contested rather than settled. The claimed type, tangled_rope, is my independent structural judgment: the arrangement carries a genuine coordination function (it resolves a real statutory ambiguity, allocates authority across federal and state levels, and gives source regulation an administrable form) and an asymmetric transfer through the same structure (coal protection paid for by climate-policy seekers and the populations the slower retirement pace burdens), held in place by active judicial enforcement. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the limit needed little enforcement while latent (2015), required stay-stage and litigation enforcement through consolidation (2017–2021), peaked at active boundary-policing after the decision (2023), and plateaus as the fenceline frame normalizes (2025). All three series share one time grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat (the Court), the arrangement is the correct allocation of a major question to the politically accountable branch — a limit on agency power, not a taking from anyone. From the beneficiary seats, it is protection of legitimate expectations, fiscal stability, and constitutional structure. From the constrained target seats (climate coalitions, EPA's drafting staff), the same text operates as a ceiling that converts a statutory phrase into a sectoral shield. From the excluded seats (harmed communities, future generations), it is a decision of the largest consequence made entirely without them. The engine computes per-seat classifications from the authored structural data; this story authors the data and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Fleet owners are declared beneficiaries with constrained exit: the ceiling directly extends their assets' operating life, so their directionality sits near the beneficiary end — their sunk-cost immobility raises what exit would cost them, not what the arrangement takes from them. Coal-producing states are beneficiaries whose exit is identity-locked: their political and fiscal identity is constituted around coal production, so even a favorable federal policy mix could not quickly detach them; they also co-set the agenda, having obtained the reading through litigation and defending it in every subsequent rulemaking. Congress is a formal beneficiary with mobile exit — it can legislate at any time — but its passivity means it collects the preserved prerogative without exercising it. Climate advocacy coalitions are targets with constrained exit: every alternative route to their preferred outcome runs under the same ceiling. Communities bearing climate harms are targets with trapped exit — they cannot relocate out of the climate system, and their interests reach the process only through the organized coalitions that represent them, which is the coalition channel for otherwise powerless seats. Renewable developers are targets with mobile exit: they lose one federal demand driver but keep their industry and its other pathways, which damps their effective extraction relative to trapped targets. EPA is a target on the authority dimension — the limit caps precisely the discretion it administers — while remaining the administrator of what the limit leaves. Future generations hold no derivation weight but anchor the transfer function: the arrangement's deferred costs land on them. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an agency restructuring a major economic sector on an ambiguous statutory phrase — is contested rather than resolved: the accountability concern is real in the sense that Congress has not enacted the comprehensive scheme the reading says it must, while the sectoral-existential problem the beneficiaries cite is being solved by market forces regardless of the reading's operation. The tangled_rope classification prevents two opposite mislabels. Reading the arrangement as pure extraction would erase the genuine coordination work: the statutory ambiguity was real, the administrability and reviewability gains are real, and the authority-allocation position is held in good faith by its adherents across the academy. Reading it as pure coordination would erase the asymmetric transfer: the same structure that resolves ambiguity shields a specific sector at the specific expense of climate-policy seekers and the populations the slower retirement pace burdens. The classification's stability depends on two open questions tracked in the omegas: if Congress exercises the reserved authority (congress_exercise_likelihood), the coordination story strengthens and the extraction component shrinks; if the clear-statement method proves selectively applied (mqd_selective_application), the coordination story collapses into cover and the arrangement drifts toward pure extraction. There is no atrophied-function decay here — the limit is young, actively enforced, and consequential — but the founding problem's contested status means the arrangement's legitimacy rests on a problem whose reality its beneficiaries alone cannot establish.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_111d,
    'This constraint is one reading of the caa_section_111d_delegation kernel — the facility_constraint_reading, under which Section 111(d)''s ''best system of emission reduction'' reaches only measures implementable at individual sources. What would change structurally if the sibling reading, caa_section_111d_delegation__systemic_transformation_reading (grid-wide generation-shifting authority including renewable substitution and early coal retirement), were adopted instead?',
    'Congressional amendment of Section 111(d), or a future Supreme Court decision revisiting the 2022 holding''s scope; observable in the near term by how subsequent rulemakings and reviewing courts define the ''system'' term.',
    'Under the sibling reading the beneficiary and target sets invert: the existing coal fleet becomes the regulated object of forced-retirement schedules, coal-producing states lose mix autonomy, and climate advocacy coalitions move to the beneficiary seat. Every directionality value and the classification itself would be re-derived from the inverted structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_111d, conceptual, 'Committer structure: this story instantiates the facility-level reading of the Section 111(d) delegation kernel; the systemic reading is its sibling and would invert this story''s beneficiary/victim structure.').

omega_variable(
    mqd_selective_application,
    'Is the clear-statement rule that produces this limit applied evenhandedly across regulatory domains, or selectively — invoked against climate rules while comparable assertions of transformative agency authority in other domains pass without it?',
    'Comparative coding of clear-statement and major-questions invocations and outcomes across domains since 2022 (student debt, vaccine mandates, financial regulation, climate rules), including which coalitions invoke it and which agencies it binds in practice.',
    'Selective application would indicate the accountability rationale is doing cover work for sectoral protection, raising this arrangement''s effective extraction and drifting its classification toward pure extraction with accountability and federalism operating as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mqd_selective_application, empirical, 'Whether the interpretive limit''s stated accountability rationale matches its actual pattern of application.').

omega_variable(
    federalism_sincerity_ambiguity,
    'Is the state autonomy the limit preserves a genuine federalism commitment, or an asymmetric shield that operates only when the protected generation mix is coal?',
    'Observe whether the same state coalitions invoke state autonomy against federal policies they otherwise favor — federal clean-energy standards, transmission-siting preemption, federal renewable mandates — or only against policies that threaten coal.',
    'Asymmetric invocation would shrink the arrangement''s coordination component and grow its extraction component; the classification would drift toward pure extraction, and the state-autonomy vindicated proposition would be re-read as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_sincerity_ambiguity, empirical, 'Sincerity test for the federalism rationale anchoring the facility reading.').

omega_variable(
    fenceline_boundary_stability,
    'Where exactly does the ''implementable at the individual facility'' boundary sit — is plant-level carbon capture inside it, is fleet-wide averaging across a company''s portfolio inside it, is a remaining-useful-life or retirement schedule inside it — and is the boundary stable across administrations and reviewing courts?',
    'Track subsequent existing-source rulemakings and the litigation over them: which compliance mechanisms survive review, and whether the accepted set expands or contracts across political administrations.',
    'Boundary migration changes who bears the limit: a narrower boundary extracts more from climate-policy seekers; a wider boundary (accepting fleet-level measures) shifts burden back toward the fleet and partially restores the sibling reading''s reach without formally adopting it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fenceline_boundary_stability, empirical, 'Stability of the source-level boundary that defines the facility reading''s reach.').

omega_variable(
    congress_exercise_likelihood,
    'Will Congress actually exercise the lawmaking power the limit reserves to it, or does the reservation operate in practice as a standing veto on federal climate action?',
    'Post-2022 legislative record: introduction, committee action, and passage of comprehensive climate or clean-electricity statutes under varying chamber compositions, plus the lobbying pattern around them.',
    'If Congress never legislates, the arrangement''s coordination story — accountability through congressional decision — loses its mechanism and the limit functions as a pure ceiling, pushing the classification toward pure extraction; if Congress legislates, the allocation story is vindicated and the extraction component shrinks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congress_exercise_likelihood, empirical, 'Whether the reserved congressional pathway is a live coordination mechanism or a nominal one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_facility_tr_t2015, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(caa_111d_facility_tr_t2017, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(caa_111d_facility_tr_t2019, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement(caa_111d_facility_tr_t2021, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(caa_111d_facility_tr_t2023, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement(caa_111d_facility_tr_t2025, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(caa_111d_facility_be_t2015, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(caa_111d_facility_be_t2017, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2017, 0.3).
narrative_ontology:measurement(caa_111d_facility_be_t2019, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement(caa_111d_facility_be_t2021, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(caa_111d_facility_be_t2023, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2023, 0.58).
narrative_ontology:measurement(caa_111d_facility_be_t2025, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_facility_su_t2015, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(caa_111d_facility_su_t2017, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2017, 0.32).
narrative_ontology:measurement(caa_111d_facility_su_t2019, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(caa_111d_facility_su_t2021, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2021, 0.47).
narrative_ontology:measurement(caa_111d_facility_su_t2023, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(caa_111d_facility_su_t2025, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the caa_section_111d_delegation kernel decomposes into two ε-invariant readings. This story authors the facility_constraint_reading — the fenceline-limit arrangement — with its own ε, its beneficiaries (coal fleet, coal states, Congress's reserved prerogative), and its victims (climate-policy seekers, climate-harmed communities, renewable developers' foreclosed pathway, EPA's capped discretion). The sibling, caa_section_111d_delegation__systemic_transformation_reading, authors the generation-shifting-authority arrangement, under which the coal fleet is the regulated object and climate-policy seekers hold the beneficiary seat; its ε is authored from its own structure. The readings are linked rather than merged because measuring the delegation one way yields low extraction and measuring it the other yields high — under the ε-invariance principle that is two constraints, not one. Direction of influence: the facility reading is currently upstream in doctrine (the 2022 holding governs subsequent existing-source rulemakings and their review), so it shapes the operating environment of any revival attempt of the systemic reading; the foreclosure declared in cs_structure holds within any single interpretive framework of the statute, while the two remain live political positions held by different coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
