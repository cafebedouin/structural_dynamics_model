% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant — Shared Infrastructure Coordination Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the HOA covenant
 *   kernel: the covenant is read as a mechanism that solves a genuine
 *   collective-action problem around shared infrastructure — roads, drainage,
 *   retaining walls, common utility trunks — that no individual owner can
 *   maintain alone and that generates real externalities when neglected.
 *   Under this reading, enforcement is narrowly scoped to cost recovery from
 *   non-payers and correction of objective, measurable hazards (blocked
 *   drains, structural failure, fire-risk vegetation), not aesthetic or
 *   behavioral conformity. This is a deliberately separate constraint from
 *   the behavioral_control_reading (which reads the same covenant text as an
 *   aesthetic-conformity enforcement mechanism) and the extraction_reading
 *   (which reads it as a revenue/power-consolidation vehicle via fine
 *   proliferation). Per the ε-invariance principle, these are not the same
 *   constraint measured three ways — they are three constraints sharing a
 *   kernel text, linked via network.affects_constraints, each with its own ε.
 *
 * KEY AGENTS:
 *   - all_homeowners: primary beneficiary and payer (moderate/constrained) — funds and receives the coordination good
 *   - hoa_board: agenda_setter (organized/constrained) — administers assessments and enforcement, itself composed of homeowners subject to periodic election
 *   - chronic_free_riders: payer (moderate/constrained) — bears cost-recovery enforcement for withheld assessments
 *   - adjacent_municipality: observer (institutional/analytical) — benefits from privatized infrastructure cost internalization without participating in governance
 *   - prospective_buyers: excluded (powerless/mobile) — inherit obligations with no current voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.16).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.22).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant — Shared Infrastructure Coordination Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'cc2b0316-bf75-4e2b-a3fe-c5a96de77978').
narrative_ontology:cs_kernel_codification('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', formalized).
narrative_ontology:cs_authority_grounding('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', practice).
narrative_ontology:cs_interpretation_layer_present('cc2b0316-bf75-4e2b-a3fe-c5a96de77978').
narrative_ontology:cs_reading_relation('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', foundational, enforcement_scope_limited_to_objective_externalities).
narrative_ontology:cs_axiom_status(enforcement_scope_limited_to_objective_externalities, holdable).
narrative_ontology:cs_axiom_grounding('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', enforcement_scope_limited_to_objective_externalities, instrumental).
narrative_ontology:cs_axiom('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', secondary, elected_board_tenure_disciplines_mandate_drift).
narrative_ontology:cs_axiom_status(elected_board_tenure_disciplines_mandate_drift, holdable).
narrative_ontology:cs_axiom_grounding('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', elected_board_tenure_disciplines_mandate_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', narrow_infrastructure_cost_recovery_mandate).
narrative_ontology:cs_drift_state('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', contemporary_hoa_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cc2b0316-bf75-4e2b-a3fe-c5a96de77978', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, chronic_free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pay regular assessments into a shared fund that maintains roads, drainage, retaining walls, shared landscaping, and common utilities that no single owner could economically maintain alone. In exchange they receive functioning shared infrastructure and protection from a neighbor's failure to maintain a party wall, drainage easement, or fire-risk vegetation from spilling costs onto them. Exit means selling the home; while resident, the assessment is the price of the coordination good.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Elected from and by the homeowners themselves, the board sets assessment levels, commissions infrastructure repairs, and enforces the objective nuisance provisions (drainage, structural hazards, fire clearance). Board members are also assessment-payers and infrastructure-beneficiaries; their tenure is limited and reviewable at annual elections, which caps how far they can drift from the coordination mandate before being replaced.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, constrained, local).

% Owners who withhold assessments while continuing to use shared roads, drainage, and common areas maintained by everyone else's contributions. The covenant's lien and collection mechanism exists to convert their free-riding into recovered cost; from their seat the enforcement lands as extraction, but the structural function is closing an externality gap they created.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, chronic_free_riders, payer,
    moderate, biographical, constrained, local).

% Benefits from the covenant privately internalizing infrastructure costs that would otherwise fall to municipal budgets or generate public nuisance complaints (flooding, unmaintained retaining walls, fire hazard). Does not participate in HOA governance but its own capital planning assumes the covenant's maintenance function continues.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, adjacent_municipality, observer,
    institutional, generational, analytical, regional).

% Not yet bound by the covenant; they evaluate disclosed assessment history and reserve-fund health before purchase but have no voice in current governance. Their only leverage is declining to buy into developments where the coordination function looks captured or underfunded — a signal that, in this reading, disciplines the board toward the narrow maintenance mandate.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, prospective_buyers, excluded,
    powerless, immediate, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared infrastructure — private roads, storm drainage, retaining walls, common utility trunks, shared fire-clearance zones — has genuine externalities: one owner's neglect degrades a system every neighbor depends on, and no individual owner can unilaterally fund or coordinate the fix. The covenant pools assessments and centralizes maintenance decisions so the collective-action problem is solved once rather than litigated house by house.
% TRANSFER_FUNCTION: Moves regular assessments from all owners into a shared maintenance fund, and moves a targeted, cost-recovery-scaled charge from owners who fail to pay or who create objective infrastructure hazards (blocked drainage, structural neglect, fire-risk vegetation) back into that same fund.
% ABSENT_VOICES: Prospective buyers have no vote in current assessment or enforcement decisions despite inheriting the obligations; they rely on disclosure requirements and reserve-fund audits rather than participation. Renters within owner-occupied units are also bound by covenant conditions with no vote at all, though this reading treats their absence as a distinct, unresolved question rather than evidence against the coordination function itself.
% DISAPPEARANCE_RATIONALE: If the covenant vanished, shared roads and drainage would have no funding mechanism; individual owners would face collective-action failure (each waiting for someone else to pay for the shared retaining wall or storm drain), infrastructure would degrade, and the municipality would likely be forced to either annex the maintenance burden or tolerate visible decay and nuisance spillover into public rights-of-way.
% FOUNDING_PROBLEM: Shared physical infrastructure (private roads, drainage systems, retaining structures, common utility runs) serving multiple parcels has no natural single owner and produces classic externalities: any owner's neglect imposes costs on neighbors, and voluntary ad hoc cost-sharing breaks down under free-riding.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning departments and civil engineers external to the HOA attest that private shared infrastructure (drainage, retaining walls, unaccepted roads) remains a genuine unsolved coordination problem wherever it exists outside a maintenance covenant — this is documented in municipal infrastructure-liability literature independent of any HOA's own justification for its assessments.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.16, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.16) because, under this reading, transfers track actual infrastructure cost and objective hazard correction rather than discretionary fines — the board's own tenure is capped by election, which structurally limits drift from the coordination mandate. Suppression is moderate-low (0.22): enforcement exists (liens, cost-recovery mechanisms) but is scoped to non-payment and objective nuisances, not to behavioral or aesthetic compliance, so the coercive apparatus is narrower than in the sibling readings. Theater ratio is low (0.1) because maintenance activity under this reading is substantially functional — inspections and assessments track real infrastructure conditions rather than performative compliance theater. Resistance is low (0.15) because the arrangement is largely accepted as legitimate cost-sharing by the governed population.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are declared symmetric beneficiaries and payers — the same agents fund the good and receive it, which is the classic rope signature. Chronic free-riders are the sole victim group, and their extraction is corrective (recovering costs they would otherwise externalize onto paying neighbors) rather than predatory. The board sits close to the beneficiary population because board members are themselves assessment-paying homeowners with limited tenure, which dampens any capture dynamic relative to a permanently entrenched agenda-setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncoordinated shared infrastructure with externality risk) is live and corroborated by parties outside the HOA's own governance (municipal planning perspectives on unmaintained private infrastructure), which is what distinguishes this reading from a mandatrophy case — the mandate has not outlived its function under this framing. If the founding problem were dead (infrastructure fully municipalized, no genuine coordination need remaining) while the covenant persisted and expanded scope, that would be the signature this reading is structurally distinct from — and is exactly the divergence the behavioral_control_reading and extraction_reading are authored to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_scope_boundary_ambiguity,
    'Where exactly does the covenant''s enforcement scope stop being genuine infrastructure/externality coordination and start being discretionary behavioral or revenue enforcement? The same enforcement clause (e.g., ''maintain property in good condition'') can be read narrowly (structural hazard, drainage) or broadly (paint color, landscaping style, holiday decorations).',
    'Audit the board''s actual violation log over several years and classify each cited violation as objective-hazard/cost-recovery versus aesthetic/behavioral versus fine-revenue-driven. A covenant whose enforcement log is dominated by objective infrastructure and cost-recovery items supports this reading; a log dominated by aesthetic citations or fine volume supports one of the sibling readings.',
    'If the empirical enforcement pattern skews heavily toward aesthetic or fine-revenue activity, this coordination_reading would be descriptively wrong for the community it purports to describe, and the behavioral_control_reading or extraction_reading would be the operative constraint for that HOA rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_scope_boundary_ambiguity, empirical, 'Whether real-world enforcement logs actually stay within the narrow coordination scope this reading claims.').

omega_variable(
    board_election_check_effectiveness,
    'Does periodic board election actually constrain drift toward extraction or behavioral overreach, or is voter turnout too low and information too asymmetric for election to function as a real check?',
    'Compare HOA annual election turnout and contested-seat rates against assessment growth rate and violation-fine revenue growth over the same period; low turnout combined with rising extraction-like metrics would suggest the election check is nominal rather than functional.',
    'If the election check is nominal, the directionality logic placing the board close to the beneficiary population is too generous, and the board should be treated more like an entrenched agenda-setter independent of the homeowner population it nominally represents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(board_election_check_effectiveness, empirical, 'Whether the board''s homeowner-elected structure genuinely disciplines its behavior toward the coordination mandate.').

omega_variable(
    kernel_framing_alternative_reading,
    'Is the covenant text itself best modeled as a single kernel with three readings, or are there really two distinct legal instruments bundled together (an infrastructure-maintenance easement/assessment agreement, and a separate architectural-review/behavioral-conduct code) that happen to be administered by the same board and thus appear as one kernel?',
    'Examine the covenant''s originating legal documents (CC&Rs) for whether infrastructure-assessment provisions and behavioral/aesthetic provisions were adopted as separable articles with separable amendment procedures, versus a single undifferentiated grant of board discretion.',
    'If the provisions are legally separable, the three readings may not be readings of one kernel at all but descriptions of two or three formally distinct constraints that merely share an administering board — which would change how the network edges among the three sibling stories should be interpreted (shared administrator rather than shared kernel).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading, conceptual, 'Whether the kernel is genuinely one contested text or an administrative bundling of separable instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__coordination_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__coordination_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__coordination_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__coordination_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__coordination_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.155).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hoa_covenant_scope kernel. coordination_reading (this file) authors low ε (0.16) reflecting narrow, cost-recovery-scoped enforcement and symmetric beneficiary structure. behavioral_control_reading and extraction_reading author substantially higher ε reflecting aesthetic-conformity enforcement and revenue/power-consolidation dynamics respectively, with asymmetric victim structures (owners subject to selective fine enforcement). The three do not average into one 'true' ε for the covenant — each is a structurally distinct claim about what the same legal instrument is doing, and per the ε-invariance principle each gets its own file, own stakeholders, and own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
