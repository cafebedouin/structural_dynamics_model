% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Interface-Boundary Reading of the GPL Derivative-Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint instantiates the interface-boundary reading of the
 *   contested GPL derivative-work trigger kernel: that clean, stable API
 *   boundaries constitute non-derivative aggregation even when the coupling
 *   between GPL and non-GPL components is functionally tight. Under this
 *   reading, the license's viral scope stops at well-documented interfaces
 *   regardless of how essential the interoperating proprietary code is to the
 *   running system. This is presented as scaffolding for modular software
 *   architecture — a practical accommodation letting GPL-licensed cores
 *   participate in mixed-license ecosystems — but it also determines who
 *   captures value at the boundary and who loses visibility into the
 *   functional whole they depend on. Sibling readings of the same kernel
 *   (broad_copyleft_reading, narrow_linking_permissive_reading) are separate
 *   constraints with their own ε and stakeholder structures; this story does
 *   not average across them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: Primary beneficiary (organized/mobile) — ships mixed-license products across the boundary
 *   - proprietary_extension_developers: Powerful beneficiary/agenda_setter (powerful/arbitrage) — architects code specifically to preserve the boundary
 *   - downstream_users_expecting_full_source: Primary target (powerless/trapped) — loses source access to functionally integral components
 *   - fsf_aligned_copyleft_advocates: Secondary target/excluded (organized/constrained) — sees copyleft's protective reach narrowed
 *   - package_maintainers: Agenda-setter (organized/constrained) — makes the architectural calls that instantiate the boundary case by case
 *   - courts_and_licensing_bodies: Analytical observer (institutional/analytical) — sparse, fact-specific adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.35).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Interface-Boundary Reading of the GPL Derivative-Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da').
narrative_ontology:cs_kernel_codification('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', fixed_text).
narrative_ontology:cs_authority_grounding('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', practice).
narrative_ontology:cs_interpretation_layer_present('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da').
narrative_ontology:cs_reading_relation('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', foundational, interface_stability_defines_derivation_boundary).
narrative_ontology:cs_axiom_status(interface_stability_defines_derivation_boundary, holdable).
narrative_ontology:cs_axiom_grounding('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', interface_stability_defines_derivation_boundary, conventional).
narrative_ontology:cs_axiom('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', secondary, functional_coupling_intensity_is_not_dispositive).
narrative_ontology:cs_axiom_status(functional_coupling_intensity_is_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', functional_coupling_intensity_is_not_dispositive, instrumental).
narrative_ontology:cs_reference_frame('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', modular_architecture_industry_practice).
narrative_ontology:cs_drift_state('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', post_saas_cloud_native_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e7f6f1c-7ce7-4eaa-bd45-c4b79f57f8da', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, commercial_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_extension_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, fsf_aligned_copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build proprietary modules that communicate with GPL-licensed cores through defined APIs, RPC calls, or plugin interfaces rather than static/dynamic linking that copies code into a single binary. They rely on the interface-boundary reading to keep their own code closed while still shipping products that interoperate with copyleft components. If the broad reading prevailed, they would have to either relicense their modules or exit the ecosystem.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    organized, biographical, mobile, global).

% Sell plugins and extensions to GPL platforms across a well-specified interface layer. Their business model depends on the interface being read as a boundary that stops derivative-work classification from propagating into their code. They lobby maintainers and license-compliance bodies to keep interface documentation stable and legally defensible.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, commercial_plugin_vendors, beneficiary,
    moderate, biographical, mobile, global).

% Large firms shipping mixed-license stacks that architect their products specifically around the interface boundary — designing plugin systems, sandboxing, and IPC layers to keep proprietary logic on the far side of the line the reading draws. They actively fund legal opinions and standards work that entrench this reading, and can restructure code to preserve the boundary if litigation pressure rises.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_extension_developers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_extension_developers, agenda_setter).

% Users and downstream redistributors who believed the GPL guaranteed access to the complete source of the functional whole they run, including tightly-coupled proprietary modules. Under this reading they receive source only for the GPL core, not the interoperating proprietary components, even where the practical coupling is dense enough that they cannot meaningfully modify or audit the system without the closed pieces. They have no direct legal remedy short of costly litigation over where the boundary actually sits in a specific codebase.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users_expecting_full_source, payer,
    powerless, biographical, trapped, global).

% Free software organizations and copyleft-committed developers whose theory of the GPL's viral protection depends on the derivative-work trigger reaching tightly-coupled interoperation, not just literal code copying. Every interface-boundary win narrows what copyleft can durably protect, undermining the anti-enclosure purpose they built the license to serve. They contest the reading in essays, license FAQs, and amicus positions but do not control the courts that would settle it.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, fsf_aligned_copyleft_advocates, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, fsf_aligned_copyleft_advocates, excluded).

% Maintainers of GPL projects who must decide, case by case, whether a given API surface counts as a genuine boundary or a thin wrapper designed to evade copyleft. Their architectural choices (stable vs. unstable APIs, plugin sandboxing, IPC vs. in-process calls) determine which reading applies in practice, but they operate without binding case law and face pressure from both integrators and copyleft advocates.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, package_maintainers, agenda_setter,
    organized, biographical, constrained, global).

% Adjudicate specific disputes about whether a given coupling arrangement crosses the derivative-work line. Their rulings are sparse, fact-specific, and jurisdiction-bound, leaving the interface-boundary reading operating mostly as an industry practice rather than a settled legal rule.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_licensing_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_extension_developers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows GPL-licensed cores and proprietary or differently-licensed modules to interoperate through stable, documented interfaces, enabling plugin ecosystems, commercial extensions, and mixed-license architectures without forcing full relicensing of every component that talks to the core.
% TRANSFER_FUNCTION: Moves the benefit of copyleft's viral protection away from downstream users and toward the developers of proprietary modules that sit across the interface boundary — users lose visibility and modification rights over functionally integral components that the broad reading would have brought under copyleft.
% ABSENT_VOICES: End users who never read a license FAQ and simply expect that software built on a 'free' core is fully inspectable; they are not present in the standards discussions or legal filings that settle where the API boundary sits, and their expectations are shaped by GPL's public reputation rather than its adjudicated scope.
% DISAPPEARANCE_RATIONALE: If the interface-boundary reading vanished and the broad copyleft reading governed instead, entire categories of commercial plugin and mixed-license architecture would face relicensing demands or would have to migrate away from GPL cores entirely — ecosystem integrators would restructure products, some commercial plugin markets would collapse or move to permissively-licensed alternatives, and copyleft's practical reach would expand substantially.
% FOUNDING_PROBLEM: The GPL's derivative-work trigger was built to prevent proprietary capture of copyleft code — closing the loophole where someone takes free code, extends it minimally, and ships a closed product that captures the value of the shared commons without returning improvements.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and copyleft advocates attest the anti-capture problem remains fully live and that interface-boundary architectures are often engineered specifically to evade it. Independent legal scholars analyzing GPL litigation history (e.g. academic commentary on the scarce case law addressing linking and interoperation) note the doctrine was never authoritatively settled at this granularity, and that industry practice — not adjudicated law — has filled the gap in ways that favor whichever party can afford to architect around the ambiguity.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) — this reading transfers real value (visibility, modification rights, copyleft protection) away from users and toward integrators, but it does so through a plausible and increasingly institutionalized architectural distinction rather than through coercive enclosure. Suppression is moderate (0.35): the reading is enforced mainly through licensing opinions, contract terms, and architectural conventions rather than direct coercion, though users have essentially no recourse to contest specific boundary placements. Theater ratio is modest but rising over the interval (0.12 to 0.28) reflecting the increasing use of interface design specifically to perform boundary-compliance rather than for its own engineering merits — an emerging Goodhart dynamic where API design is optimized to win the classification argument, not to serve genuine modularity.
 *
 * PERSPECTIVAL GAP:
 *   From the ecosystem integrator seat, this reading is a rope: a genuine coordination solution enabling mixed-license innovation without which large parts of the plugin and extension economy could not exist. From the downstream user seat, the same structure computes closer to extractive: they lose the transparency and modification rights the license's public reputation promised them, without any say in where a given interface is deemed to constitute a 'boundary.' The scaffold framing sits between these: the story claims scaffold because the reading is meant to enable transitional modular architecture pending clearer doctrine, but the engine may compute differently from the metrics — that divergence is itself the signal, not an error to correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators, commercial plugin vendors, and proprietary extension developers derive low d — they are structural beneficiaries who designed their systems around this reading and can exit toward alternative architectures or licenses if it fails (arbitrage/mobile exit options reinforce the low-d derivation). Downstream users expecting full source derive high d — trapped exit, no negotiating power, they bear the cost of narrowed disclosure with no meaningful alternative. FSF-aligned advocates sit at moderate-high d: organized and vocal but without direct legal or architectural control over how the boundary gets drawn in any specific codebase.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim distinguishes this reading from either a pure rope (which would require the coordination benefit to be net-positive for all parties without suppression) or a pure snare (which would require no genuine coordination function at all). The interface-boundary reading has a real coordination function — modular architecture genuinely benefits from stable interface contracts — but it also has increasingly performative elements (theater_ratio rising) where boundary design is optimized for legal defensibility rather than engineering necessity. Declaring has_sunset_clause reflects that this reading is meant to hold until clearer doctrine or updated license language (e.g. explicit interface-boundary carve-outs, or a definitive appellate ruling) resolves the underlying kernel ambiguity — it is not intended as a permanent settlement, which is exactly the scaffold's justificatory structure: the transition, not the steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_thinness_ambiguity,
    'How thin can an API layer be before it stops functioning as a genuine architectural boundary and becomes a thin wrapper designed purely to evade the derivative-work trigger?',
    'Case-by-case judicial or expert-technical analysis of specific interface implementations — degree of abstraction, stability of the interface contract, whether the interface predates the coupling or was designed around it, and whether the interface could support multiple independent implementations.',
    'If courts adopt a strict substance-over-form test, many currently-compliant architectures under this reading would reclassify toward the broad_copyleft_reading; if courts defer to formal interface boundaries regardless of thinness, this reading''s practical scope would expand further and extraction from downstream users would rise correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_thinness_ambiguity, empirical, 'Where the line sits between genuine architectural boundary and boundary-shaped evasion.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the interface-boundary reading the operative legal/industry norm, or is it primarily an industry-convenience convention that has not been meaningfully tested against the broad_copyleft_reading in courts with jurisdiction over the relevant codebases?',
    'Survey of adjudicated GPL linking/interoperation cases across jurisdictions, cross-referenced against industry compliance practice; absence of contrary rulings is weak evidence for the reading''s dominance, not proof of its correctness.',
    'If courts have simply never ruled against this reading due to settlement incentives and litigation cost asymmetry favoring well-resourced integrators, the reading''s apparent stability is a product of enforcement asymmetry rather than legal merit — this would shift the omega toward the beneficiary-favoring interpretation being a constructed convention rather than settled doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether industry adoption of this reading reflects legal merit or litigation-cost asymmetry.').

omega_variable(
    sunset_trigger_ambiguity,
    'What would actually trigger the scaffold''s sunset — a definitive appellate ruling, an FSF-led license revision (e.g. GPLv4 language explicitly addressing APIs), or gradual convergence of industry practice into de facto settled doctrine?',
    'Track license revision proposals, pending litigation dockets, and industry standards body activity (e.g. SPDX, OSI) for movement toward a definitive interface-boundary test.',
    'If no credible sunset mechanism exists, the has_sunset_clause declaration is aspirational rather than structural, and the constraint may function more like an indefinitely persisting arrangement than a genuine transition — which would argue for piton rather than scaffold on renewed assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_trigger_ambiguity, conceptual, 'Whether a credible mechanism exists to resolve the scaffold into settled doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_derivative_work_trigger__interface_boundary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.1).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% Part of the gpl_derivative_work_trigger constraint family (3 readings of one kernel). broad_copyleft_reading treats any linking as derivation (highest ε for users, lowest for integrators); narrow_linking_permissive_reading treats linking as aggregation absent direct modification (lowest ε for users, highest for integrators); this interface_boundary_reading sits between them, conditioning its boundary test on interface design quality rather than link mechanism alone. Each reading is a separate constraint with its own stable ε; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
