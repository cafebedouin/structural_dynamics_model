% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member Sovereignty Reading of Free Movement — Welfare Capacity Boundary
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This is the member_sovereignty_reading of the
 *   federation_membership_kernel: free movement is real and valuable, but it
 *   is bounded, not absolute — member states retain the authority to
 *   condition access to national welfare and labor markets on economic
 *   activity, and to exclude or remove economically inactive migrants who
 *   lack sufficient resources or genuine work-seeking status. This reading
 *   treats the welfare state as a national solidarity institution whose
 *   actuarial and political integrity depends on a defined contributory
 *   population; unbounded free movement is read as a structural threat to
 *   that integrity, not merely friction to be minimized. The reading is
 *   authored on its own terms and its ε reflects the standing arrangement of
 *   conditioned mobility as this reading itself experiences and defends it —
 *   not the integration_reading's account of the same treaty text, and not
 *   the welfare_coordination_reading's narrower anti-social-dumping framing.
 *   Those are separate constraints (see kernel_context and network links)
 *   with their own ε and stakeholder sets.
 *
 * KEY AGENTS:
 *   - national_governments_asserting_sovereignty: administers eligibility screening and derogation authority (institutional/analytical)
 *   - receiving_state_welfare_institutions: beneficiary of preserved actuarial bounds (institutional/analytical)
 *   - receiving_state_incumbent_workers: beneficiary of reduced labor-supply pressure (organized/constrained)
 *   - sending_state_mobile_workers: bear documentation burden and conditional protection (powerless/constrained)
 *   - economically_inactive_migrants: directly excluded and removable (powerless/trapped)
 *   - sending_state_labor_markets: bear intensified brain drain (moderate/constrained)
 *   - european_court_of_justice: interpretive authority narrowed under this reading (institutional/analytical, excluded)
 *   - eu_commission: sidelined enforcement preference (institutional/analytical, excluded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.51).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member Sovereignty Reading of Free Movement — Welfare Capacity Boundary").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '390a44aa-299f-4648-b068-fa6ff314a35b').
narrative_ontology:cs_kernel_codification('390a44aa-299f-4648-b068-fa6ff314a35b', fixed_text).
narrative_ontology:cs_authority_grounding('390a44aa-299f-4648-b068-fa6ff314a35b', distributed).
narrative_ontology:cs_reading_relation('390a44aa-299f-4648-b068-fa6ff314a35b', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('390a44aa-299f-4648-b068-fa6ff314a35b', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('390a44aa-299f-4648-b068-fa6ff314a35b', foundational, national_welfare_solidarity_bounded_by_membership).
narrative_ontology:cs_axiom_status(national_welfare_solidarity_bounded_by_membership, holdable).
narrative_ontology:cs_axiom_grounding('390a44aa-299f-4648-b068-fa6ff314a35b', national_welfare_solidarity_bounded_by_membership, conventional).
narrative_ontology:cs_axiom('390a44aa-299f-4648-b068-fa6ff314a35b', foundational, member_state_retains_residual_sovereignty_over_social_protection).
narrative_ontology:cs_axiom_status(member_state_retains_residual_sovereignty_over_social_protection, holdable).
narrative_ontology:cs_axiom_grounding('390a44aa-299f-4648-b068-fa6ff314a35b', member_state_retains_residual_sovereignty_over_social_protection, conventional).
narrative_ontology:cs_reference_frame('390a44aa-299f-4648-b068-fa6ff314a35b', treaty_derogation_authority_baseline).
narrative_ontology:cs_drift_state('390a44aa-299f-4648-b068-fa6ff314a35b', post_dano_jurisprudence_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('390a44aa-299f-4648-b068-fa6ff314a35b', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_governments_asserting_sovereignty).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers residence and social-benefit eligibility rules, screens for 'sufficient resources' and 'genuine work-seeker' status, and can deport economically inactive migrants after a defined period. Justifies the gatekeeping as necessary to protect the fiscal integrity of national welfare institutions and domestic labor markets. Retains the treaty-recognized derogation authority and administers it directly.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_governments_asserting_sovereignty, agenda_setter,
    institutional, generational, analytical, national).

% Pension funds, unemployment insurance schemes, and social assistance programs designed around a bounded contributory base. The exclusion of economically inactive migrants and benefit-tourism screening preserves actuarial assumptions and public legitimacy for the solidarity system without requiring redesign.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions, beneficiary,
    institutional, generational, analytical, national).

% Domestic labor unions and workers benefit from reduced downward wage pressure and slower labor-supply expansion when inflows of economically inactive or benefit-seeking migrants are restricted. They lobby to maintain and tighten the exclusion criteria as protection against wage compression and service-queue crowding.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers, beneficiary,
    organized, biographical, constrained, national).

% Workers from lower-wage member states who wish to relocate for employment or family reasons face documentation burdens, waiting periods, and 'genuine prospect of work' tests before social protection attaches. Many delay migration, migrate informally, or accept precarious work below their qualification level to avoid falling into the excluded economically-inactive category. Exit from the home labor market is possible but arrival protection in the destination state is conditional and revocable.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_mobile_workers, payer,
    powerless, biographical, constrained, continental).

% Retirees, jobseekers who exhaust the search period, and family members without independent income are directly excluded from host-state social assistance and can be required to leave. They have crossed a border in good-faith reliance on free movement rights and now find the safety net conditional on economic activity status they cannot always control (illness, caregiving, redundancy).
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, national).

% Lower-wage member states lose skilled and working-age population to more restrictive, selective outmigration patterns shaped by this reading's screening criteria — workers who meet the destination state's economic-activity thresholds leave, while those who don't stay trapped without support, distorting both the sending state's remaining workforce composition and its long-run fiscal base. Brain drain intensifies because the reading rewards immediately employable migrants and filters out others, concentrating outmigration among the most productive workers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_markets, payer,
    moderate, generational, constrained, national).

% Historically the primary interpretive authority for the scope of free movement rights, favoring expansive readings under the integration_reading. Under member_sovereignty_reading, the Court's interpretive latitude is narrowed by treaty language and Council-level political consensus reasserting member state derogation authority; its rulings are treated as reviewable rather than final by this reading's proponents.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_court_of_justice, excluded,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, european_court_of_justice, observer).

% Would prefer harmonized, expansive free movement enforcement consistent with single-market completion, but under this reading is structurally sidelined in favor of member state administrative discretion over eligibility screening; infringement proceedings against restrictive national rules face high political resistance.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_commission, excluded,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the fiscal and actuarial integrity of nationally-scoped welfare and labor-market institutions by bounding who can draw on them, allowing member states to sustain solidarity systems designed around a defined contributory population rather than an open-ended one.
% TRANSFER_FUNCTION: Moves the burden of adjustment from receiving-state welfare institutions and incumbent workers onto mobile individuals from lower-wage member states: eligibility risk, administrative burden, and income precarity are transferred to those attempting to exercise free movement, while receiving states retain the fiscal and labor-market stability benefits of restricted access.
% ABSENT_VOICES: Economically inactive migrants who have already relocated in reliance on free movement rights have no seat in the intergovernmental bargaining that sets the eligibility thresholds; sending states' domestic constituencies who bear the long-run costs of accelerated brain drain are structurally underrepresented relative to receiving-state electorates whose preferences dominate treaty renegotiation.
% DISAPPEARANCE_RATIONALE: If member state authority to bound free movement by welfare capacity and labor market protection disappeared overnight, national eligibility screening would collapse, receiving-state welfare systems would face unbounded claims from a continental population, incumbent-worker wage protections would weaken, and the political coalition currently sustaining continued EU integration (built partly on this bounded-solidarity settlement) would fracture — this is a load-bearing feature of the current federation, not incidental policy.
% FOUNDING_PROBLEM: Early expansive free movement combined with generous, nationally-designed welfare states created a structural mismatch: a right to move continent-wide attached to solidarity institutions built and funded on the assumption of a bounded national population, risking both fiscal free-riding accusations and a political backlash against the integration project itself.
% FOUNDING_PROBLEM_CORROBORATION: Receiving-state governments and their domestic labor constituencies attest the problem remains live, citing ongoing welfare-tourism litigation and public opinion data showing continued salience of migration-welfare linkage. Independent labor economists and sending-state governments outside the receiving-state beneficiary coalition attest the empirical scale of welfare-tourism was always small relative to the political framing, and that the arrangement now functions more to manage receiving-state domestic politics than to solve a demonstrated fiscal threat — a divergence documented in ECJ case commentary and Commission-commissioned mobility studies that receiving states have resisted acting on.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the reading transfers real costs — precarity, exclusion, brain drain — from receiving-state institutions and incumbent workers onto mobile individuals and sending states, but it is not maximal because a genuine coordination function (protecting actuarially bounded solidarity systems) is also served, not merely simulated. Suppression (0.51) reflects active administrative screening, residence permit conditionality, and deportation authority — real coercive machinery, not merely norm-setting. Theater ratio is low-moderate (0.28) because most of the enforcement (residence checks, sufficient-resources tests) does functional gatekeeping work rather than pure performance, though a growing share is symbolic reassurance to domestic electorates as EU-level pressure to harmonize persists. Accessibility collapse is moderate (0.42): affected migrants retain formal free movement rights and legal recourse, but practical access to protected status narrows once economic-activity screening applies. Resistance is substantial (0.61): sending states, migrant advocacy groups, and the ECJ's residual jurisprudence continue to contest the boundary this reading draws, which is precisely why this reading requires active enforcement rather than passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state welfare institutions and incumbent workers are structural beneficiaries: the constraint protects their fiscal base and labor-market position, so directionality sits near the beneficiary end. National governments are the agenda-setting seat administering the boundary; their institutional power and analytical exit options place them structurally adjacent to the beneficiary cluster even though their formal role is enforcement rather than direct collection. Sending-state mobile workers and economically inactive migrants are the targets: they bear the transfer directly, have constrained-to-trapped exit options, and their directionality sits near the full-target end. Sending-state labor markets are a diffuse victim — moderate power because states retain some bargaining leverage in EU councils, but constrained exit because renegotiating the free-movement settlement unilaterally is not realistically available to any single sending state.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mismatch between continent-wide mobility rights and nationally-bounded welfare design — remains genuinely contested rather than resolved or dead. This reading classifies as tangled_rope rather than snare precisely because the coordination function (protecting actuarial integrity of solidarity systems against unbounded claims) is real and independently defensible, not merely a cover story: a welfare system that could not bound its claimant population would face genuine fiscal and political risk. But the same structure that performs this coordination function simultaneously extracts asymmetrically from sending-state workers and inactive migrants who have no equivalent voice in setting the boundary. Treating this reading as a pure Mountain (an inevitable natural limit) would erase the asymmetric extraction; treating it as a pure Snare would erase the genuine actuarial coordination problem it solves. Tangled rope holds both facts open simultaneously, which is the correct classification for a kernel reading precisely engineered to look like principled boundary-setting from the inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_as_genuine_limit_or_captured_boundary,
    'Is the economic-activity screening this reading defends a genuine, proportionate response to demonstrated fiscal risk to welfare institutions, or is it a boundary substantially captured by incumbent-worker and welfare-institution interests that exceeds any demonstrated risk?',
    'Compare empirical welfare-tourism claim rates and fiscal cost estimates (Commission-commissioned mobility studies, national audit office reports) against the scope and severity of exclusion criteria actually enforced; a large gap between demonstrated risk and enforcement scope would indicate capture beyond proportionate coordination.',
    'If the boundary is proportionate to demonstrated risk, the tangled_rope classification''s coordination component is well-grounded. If the boundary substantially exceeds demonstrated risk, the constraint drifts toward snare — the coordination story becomes cover for protecting incumbent labor-market position rather than genuine welfare-institution solvency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reading_as_genuine_limit_or_captured_boundary, empirical, 'Whether the sovereignty-reading''s screening scope matches its stated fiscal justification.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (member_sovereignty_reading) of the federation_membership_kernel; sibling readings (integration_reading, welfare_coordination_reading) draw the free-movement/welfare-boundary line very differently from the same treaty text. Where exactly is the disagreement located — in the interpretation of treaty language itself, in empirical estimates of welfare-tourism scale, or in prior normative commitments about the primacy of mobility versus national solidarity?',
    'Trace ECJ case law evolution (e.g., Dano, Alimanovic, Brey lines of jurisprudence) against Commission enforcement patterns and Council-level political negotiation records to identify whether shifts in the boundary track new empirical evidence, changed legal doctrine, or shifted political coalitions.',
    'If the disagreement is primarily normative/political rather than doctrinal or empirical, the classification of this reading as principled coordination versus this reading as majoritarian sovereignty assertion becomes substantially a matter of which underlying commitment the observer already holds — this is exactly the committer-frame ambiguity the kernel/reading structure is designed to isolate rather than resolve within a single constraint file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating where the three kernel readings actually diverge — text, evidence, or prior normative commitment.').

omega_variable(
    brain_drain_intensification_causal_attribution,
    'How much of the intensified brain drain from sending states is causally attributable to this reading''s selective screening criteria (which favor immediately employable migrants) versus pre-existing wage and opportunity differentials that would drive outmigration regardless of the free-movement boundary regime?',
    'Comparative analysis of sending-state outmigration composition before and after tightened economic-activity screening was introduced, controlling for wage differential trends over the same period.',
    'If the screening regime is the dominant driver of selective (skill-concentrated) outmigration, sending-state labor market harm is more directly attributable to this constraint and strengthens its victim classification. If wage differentials would produce similar selective outmigration regardless, the constraint''s marginal contribution to sending-state harm is smaller than the narrative suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_intensification_causal_attribution, empirical, 'Whether screening criteria or pre-existing wage differentials drive selective brain drain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fede_tr_t4, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(fede_tr_t8, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(fede_tr_t16, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t4, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(fede_be_t8, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fede_be_t16, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(fede_su_t4, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(fede_su_t8, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(fede_su_t16, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 24, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked stories decomposing the natural-language concept 'the scope of EU free movement rights' per the ε-invariance principle. federation_membership_kernel__integration_reading authors the ECJ-favored expansive reading (low ε, near-mountain framing of mobility as near-fundamental). federation_membership_kernel__member_sovereignty_reading (this file) authors the national-discretion reading (moderate-high ε, tangled_rope). federation_membership_kernel__welfare_coordination_reading authors the intermediate coordination-without-harmonization reading (expected lower-moderate ε, rope-leaning). Each reading is generated as an independent file with its own stakeholders, metrics, and classification; they are linked here rather than merged because measuring 'free movement scope' by different observables (treaty text as ECJ interprets it vs. treaty text as Council/member states interpret it vs. treaty text as coordination mechanism) yields structurally different ε values — three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
