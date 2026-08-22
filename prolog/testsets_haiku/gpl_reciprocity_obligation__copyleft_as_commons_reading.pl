% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Copyleft Reciprocity Obligation (Commons Preservation Reading)
 *   domain: intellectual_property/software_governance/open_source
 *
 * SUMMARY:
 *   The GPL (GNU General Public License) embeds a reciprocity obligation: any
 *   modification to GPL-licensed code must be released under GPL terms if
 *   distributed. This reading interprets that obligation not as a restriction
 *   on individual freedom but as institutional technology for commons
 *   preservation. When a developer builds on GPL code and then makes a
 *   proprietary product, GPL's terms force them to either release the code,
 *   rewrite to avoid GPL dependencies, or pay for exemption. The constraint
 *   operates to prevent enclosure of shared innovation by proprietary vendors
 *   and to maintain a collectively-managed repository of freely-modifiable
 *   code. The reading's ε is medium (0.58): the constraint extracts
 *   obligation from those who want proprietary optionality, but that
 *   extraction funds a coordinated commons rather than a captured
 *   institution.
 *
 * KEY AGENTS:
 *   - Commons-as-collective-institution: the shared repository of code and governance structures GPL preserves; benefits from mandatory reciprocity by preventing enclosure
 *   - GPL upstream maintainers (Linux, GNU, Apache): agenda-setters who enforce the constraint by refusing non-GPL code; they choose GPL deliberately to bind recipients into reciprocity
 *   - Proprietary business models: powerful actors forced to pay licensing costs or exit GPL supply chains; they bear the extraction in exchange for access to commons code
 *   - Individual exit-maximizers: developers with fused identity in commons who want to privatize their modifications but are suppressed by reciprocity obligation; they bear psychological and opportunity costs
 *   - Commercial dual-licensing vendors: institutional actors who profit by offering proprietary exemptions from GPL terms; they operate within and against the commons constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.58).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Copyleft Reciprocity Obligation (Commons Preservation Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "intellectual_property/software_governance/open_source").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd').
narrative_ontology:cs_kernel_codification('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', fixed_text).
narrative_ontology:cs_authority_grounding('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', lineage).
narrative_ontology:cs_interpretation_layer_present('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd').
narrative_ontology:cs_reading_relation('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', foundational, commons_preservation_through_reciprocal_obligation).
narrative_ontology:cs_axiom_status(commons_preservation_through_reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', commons_preservation_through_reciprocal_obligation, deontological).
narrative_ontology:cs_axiom('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', foundational, shared_knowledge_as_collective_good).
narrative_ontology:cs_axiom_status(shared_knowledge_as_collective_good, holdable).
narrative_ontology:cs_axiom_grounding('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', shared_knowledge_as_collective_good, deontological).
narrative_ontology:cs_reference_frame('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', gpl_reciprocity_as_commons_anchor).
narrative_ontology:cs_drift_state('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', contemporary_patent_challenge_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('0ce81dd6-6ea7-4a52-bdae-be4211d8c3bd', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_collective_institution).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_business_models).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_derivative_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, commercial_dual_licensing_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The shared repository of freely-modifiable code that GPL's reciprocity obligation preserves. Not a legal entity but a coordination structure: when developers must release derived works under GPL, they feed improvements back into the commons, sustaining its growth and preventing enclosure by proprietary vendors. The constraint operates to maintain the commons as a collective good rather than as private capital.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_collective_institution, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_collective_institution).

% Authors and stewards of GPL-licensed codebases (Linux kernel maintainers, GNU project, Apache Foundation members, etc.). They set the licensing terms and enforce them by refusing to incorporate code that violates GPL terms. They choose GPL specifically to bind downstream recipients into reciprocal obligation. Their power derives from holding the upstream gatekeeping position and from the coordination problem GPL solves.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_upstream_maintainers, agenda_setter,
    organized, generational, mobile, global).

% Corporations and ventures that want to incorporate GPL code into proprietary products without releasing their own code. The GPL obligation to share modifications ('viral' reciprocity) forces them to either: (1) release proprietary code under GPL (loss of proprietary advantage), (2) rewrite to avoid GPL dependencies (cost and delay), or (3) negotiate a separate commercial license (payment to licensor). They bear the cost of the constraint through licensing friction and reduced business model optionality.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_business_models, payer,
    powerful, biographical, constrained, global).

% Developers who want to contribute to a GPL project, maintain private modifications, and avoid releasing them. They are trapped between their desire to use upstream code and the GPL obligation to share their own work. Their identity as software engineers may be fused with participation in open-source communities, making true exit (to proprietary-only development) psychologically costly despite the material constraint.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    moderate, biographical, identity_locked, global).

% End-users and downstream developers who receive GPL-licensed code and modifications. They benefit from the constraint's reciprocity because all improvements made by anyone are guaranteed to flow back to them; they have access to the complete source of everything that runs on their machines. Their alternatives (proprietary or closed-source software) offer no such guarantee.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_derivative_users, beneficiary,
    powerless, biographical, trapped, global).

% Companies that maintain GPL codebases but also sell commercial (non-GPL) licenses to the same code, enabling proprietary vendors to use their software without reciprocal obligation. They operate both within and against the commons constraint: they benefit from GPL's enforcement machinery (which attracts contributors and prevents enclosure by rivals) while providing an escape valve (commercial licensing) that lets them extract rents from those willing to pay for exemption.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commercial_dual_licensing_vendors, agenda_setter,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, commercial_dual_licensing_vendors, payer).

% The FSF and aligned advocacy communities maintain the ideological and legal infrastructure that sustains GPL's interpretation and enforcement. They produce GPL versions, defend it in court, educate developers about its obligations, and advocate for its use as a commons-preservation mechanism. They hold no formal legal authority but exercise substantial influence over how GPL is understood and applied.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_foundation_advocates, observer,
    organized, generational, analytical, global).

% Companies whose business models depend on code secrecy and proprietary differentiation would argue against mandatory reciprocity, asserting it infringes on their freedom to build proprietary layers on top of commons code. They are structurally excluded from the GPL commons because GPL's enforcement prevents them from using GPL-licensed code in closed products without exemption or rewrite.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_software_vendors, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_collective_institution).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the tragedy-of-the-commons problem in software: prevents any single actor from enclosing the shared codebase by taking GPL code proprietary. Coordinates all downstream users and modifiers into a reciprocal obligation that ensures improvements flow back to the collective pool rather than being privatized.
% TRANSFER_FUNCTION: Transfers the obligation to share modifications from the developer to anyone who receives their work. Moves the value of derived code from individual/proprietary holders back into the commons. Effectively transfers bargaining power from proprietary vendors (who could otherwise capture GPL code) to the commons as a coordinated whole.
% ABSENT_VOICES: Individual developers who want to keep their modifications proprietary but are not present in GPL governance structures. Proprietary software vendors who oppose mandatory reciprocity but are excluded by design from the GPL system. Companies in jurisdictions where GPL enforceability is contested or weak would argue for looser terms but have no seat at the table in GPL standard-setting.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity vanished overnight, proprietary vendors would immediately incorporate GPL code into closed products without releasing their modifications. The shared commons would begin fragmenting into proprietary variants within months. Within years, the incentive structure that sustains upstream commons projects would weaken (fewer derivative improvements flowing back, reduced visibility for commons-licensed work), causing upstream contribution to decline. The software landscape would reorganize around proprietary/permissive licensing rather than copyleft.
% FOUNDING_PROBLEM: Enclosure of shared software innovation by proprietary vendors: companies could take GPL code, modify it, build proprietary products on top, and refuse to share improvements. Individual developers and institutions building commons could see their work captured and privatized by larger actors with legal/marketing resources.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's operation is attested by: (1) upstream maintainers' explicit defense of GPL specifically to prevent enclosure (Linux kernel documentation, GNU project statements), (2) documented cases where proprietary vendors were forced to choose between releasing code or rewriting (Apple's incorporation of CUPS, various embedded-systems cases), (3) independent software economists' analysis showing GPL preserves commons dynamics. The commons-reading interpretation is corroborated by open-source governance scholarship and contributor interviews; it is NOT corroborated by proprietary vendors (who characterize GPL as restricting their freedom rather than preserving commons).
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled_rope (not rope) because: (1) it solves a genuine coordination problem (preventing enclosure), (2) it imposes asymmetric extraction on those who want proprietary optionality, and (3) it requires active enforcement (upstream maintainers policing license compliance). The metrics reflect this structure: extractiveness is medium (0.58) because the obligation is substantial but serves collective good rather than elite capture; suppression is moderate (0.42) because exit is constrained but not eliminated (proprietary vendors can rewrite or buy exemptions; developers can choose non-GPL projects); theater is low (0.15) because the enforcement machinery is functional (license checking is real, not performance). The measurement series show modest growth in extractiveness early (as GPL became standard practice in Linux and FSF projects) then plateauing as the constraint stabilized around t=15-20. Accessibility collapse is high (0.68) because once the GPL obligation is understood, alternatives (non-GPL code, proprietary rewrites) are the only viable exits, and those are costly. Resistance is also high (0.71) because proprietary vendors actively contest GPL enforceability in court and in regulation, and individual developers regularly attempt to circumvent terms.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (upstream maintainers) experiences this constraint as coordination technology they deliberately built to solve enclosure. The payer seats (proprietary vendors and exit-maximizers) experience it as restriction and obligation. From the upstream seat: 'GPL preserves the commons by binding recipients into reciprocity.' From the proprietary seat: 'GPL extracts my code and my competitive advantage.' From the exit-maximizer seat: 'I want to contribute to the commons but keep my innovations private; GPL prevents that.' From the downstream-user seat: 'I benefit because all improvements flow back to me.' These perspectives are not equally valid (the engine does not average them); each seat's perception is derived from its structural relationship to the constraint (power, exit options, beneficiary/victim status). The engine computes different types at different seats: upstream maintainers may compute rope (coordination they control), while proprietary vendors compute snare (extraction enforced against their preference).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is the structural position each agent occupies relative to the constraint: how much they are targeted (d→1.0) or subsidized (d→0.0). Upstream maintainers have low d (they control the constraint, benefit from reciprocity, have high exit optionality to license differently) — d~0.2, beneficiary-side. Proprietary vendors have high d (they are targeted by the obligation to release code or pay for exemption, constrained exit, bear costs) — d~0.75, target-side. Individual exit-maximizers have moderate-to-high d (they want to exit GPL reciprocity but are identity-locked, suppressed by obligation to share) — d~0.65, target-side with suppression overlay. Downstream users have low d (they benefit from guaranteed access to improvements, no cost to them) — d~0.15, beneficiary-side. The commons-as-institution is a non-agent beneficiary (no d, but structured as beneficiary in role) — it collects the improvements that flow back.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy (decoupling of persistence from founding-problem necessity). The founding problem (enclosure of commons code by proprietary vendors) remains live and actively contested. GPL's continued enforcement and periodic legal defense (against claims it violates patent law, contract law, copyright interpretation) show the founding problem has not been solved — proprietary vendors continue attempting to circumvent or weaken GPL terms. The mechanism persists because it is functionally necessary, not because it has become theatrical. Theater ratio is deliberately low (0.15) rather than high because the enforcement is real: license checking is a genuine gating function in upstream projects, not performative. If theater were rising toward 0.5+, that would indicate the commons was no longer actually enforced (licenses no longer checked, violations no longer rejected, theatrical gestures about licensing without actual enforcement). The stable theater ratio and plateau in extractiveness at t=20+ suggest the constraint has reached equilibrium rather than degradation, though sustained resistance (0.71) indicates the founding problem persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_institutional_status,
    'Is ''the commons'' a genuine institutional beneficiary, or is it merely a reification of the collective interests of individual developers? Is the constraint extracting FROM individuals in service to an institution, or coordinating them INTO one?',
    'Examine whether GPL''s enforcement produces observable institutional properties (governance structures, collective resource management, barriers to exit that persist after individual actors leave) that are not reducible to the aggregated preferences of participants.',
    'If commons-as-institution, the constraint coordinates with asymmetric extraction from exit-maximizers (tangled_rope). If commons-as-reification, the constraint is pure coordination (rope) with no institutional victim. The classification shifts based on whether the commons exhibits agency independent of its members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_institutional_status, conceptual, 'Whether the commons is an institutional beneficiary or a reification of collective preference.').

omega_variable(
    extraction_from_proprietary_vendors_vs_exit_maximizers,
    'Which seat actually bears the constraint''s extractive burden: proprietary business models (forced to exit GPL chains or buy commercial licenses), or individual developers (prevented from privatizing their modifications)?',
    'Measure the distribution of compliance costs: licensing expenditure by proprietary vendors vs. foregone private-gain opportunities for individual developers. Track whether enforcement prioritizes preventing proprietary enclosure or preventing individual exit.',
    'If proprietary vendors bear the primary cost, the victim set is economic actors, and extraction funds the commons. If individual developers bear it (through identity-lock and suppressed exit), extraction creates a fused identity that serves institutional reproduction. This affects whether the constraint qualifies as tangled_rope (two victims) or snare-with-institutional-capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_from_proprietary_vendors_vs_exit_maximizers, empirical, 'Distribution of extraction burden between proprietary vendors and individual developers.').

omega_variable(
    reading_boundary_commons_vs_freedom,
    'Is this reading of GPL fundamentally distinct from the copyleft_as_freedom_reading, or do they describe the same constraint from different normative angles?',
    'Examine whether the two readings would produce different ε values or different victim sets under identical factual conditions. The freedom reading emphasizes prevention of proprietary capture of user freedoms; the commons reading emphasizes prevention of institutional enclosure. If both readings agree on who bears costs and who benefits, they are the same constraint with different narratives (should be merged). If they disagree on beneficiaries or victims, they are truly distinct constraints (remain separate).',
    'If distinct: two separate constraint stories linked by affects_constraints. If identical: redundant reading that should be collapsed into the freedom reading. The ε-invariance principle turns on whether beneficiary/victim sets differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_commons_vs_freedom, conceptual, 'Whether the commons and freedom readings are structurally distinct constraints or variant frames of one constraint.').

omega_variable(
    dual_licensing_escape_valve,
    'Do dual-licensing arrangements (GPL commons + commercial licenses) undermine the constraint''s capacity to prevent enclosure, or do they enhance it by providing an institutional legitimacy valve that reduces political pressure to weaken GPL terms?',
    'Historical counterfactual: estimate how GPL''s enforceability would change under two scenarios: (1) dual licensing permitted, (2) dual licensing prohibited. Track regulatory pressure and litigation outcomes under each regime.',
    'If dual licensing undermines enclosure prevention, ε is higher than measured (the escape valve lets proprietary capture proceed for those who pay). If it enhances institutional legitimacy, ε remains accurate or slightly lower (the constraint persists because commercial actors have a pressure-relief path). This affects whether the constraint is sustainable tangled_rope or a degrading snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_licensing_escape_valve, empirical, 'Whether dual licensing weakens or strengthens commons preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the GPL reciprocity obligation kernel family. The commons reading, freedom reading, and restriction reading all interpret the same GPL mechanism but from different institutional vantage points. Commons reading = GPL as commons-preservation technology (medium extractiveness, institutional beneficiary). Freedom reading = GPL as user-freedom preservation (lower extractiveness, individual beneficiary). Restriction reading = GPL as business-model constraint (higher extractiveness, proprietary vendors as principal victims). The three readings share a referent (the GPL mechanism) but carry distinct ε values, victim/beneficiary sets, and terminal classifications. Linked by network.affects_constraints for cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, powerful, 0.75).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
