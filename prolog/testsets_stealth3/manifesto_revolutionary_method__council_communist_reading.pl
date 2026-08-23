% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Federated Workers' Council Rule (Council-Communist Reading)
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the manifesto_revolutionary_method
 *   kernel: the council-communist claim that all power passes to federated
 *   workplace assemblies, replacing both the capitalist state and any
 *   vanguard party. The arrangement's ε referent is the standing council
 *   arrangement itself as this reading authors it — never the vanguard or
 *   gradualist alternatives. KEY AGENTS (by structural relationship): -
 *   autonomous_worker_collectives: agenda-setting beneficiary
 *   (organized/constrained) — runs the federation and collects the freed
 *   surplus; - rank_and_file_industrial_workers: primary beneficiary
 *   (organized/constrained) — gains shop-floor control, pays delegate time; -
 *   federated_peasant_communes: secondary beneficiary (organized/constrained)
 *   — federates land and grain decisions; - state_bureaucrats: primary target
 *   (institutional/trapped) — offices and command rents abolished; -
 *   vanguard_party_officials: primary target (organized/identity_locked) —
 *   directing-role claim dissolved; - non_aligned_socialist_tendencies:
 *   excluded voice (organized/constrained) — admitted in ideal, marginalized
 *   in practice; - council_communist_theorists: analytical observer — sees
 *   the full structure across episodes. The sibling readings (vanguard
 *   rupture, democratic gradualism) are separate constraint files with their
 *   own ε and victim sets; nothing about them is averaged into this story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.48).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Federated Workers' Council Rule (Council-Communist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'cb9ba65e-7074-49a2-9d86-9111dd091f1d').
narrative_ontology:cs_kernel_codification('cb9ba65e-7074-49a2-9d86-9111dd091f1d', fixed_text).
narrative_ontology:cs_authority_grounding('cb9ba65e-7074-49a2-9d86-9111dd091f1d', lineage).
narrative_ontology:cs_interpretation_layer_present('cb9ba65e-7074-49a2-9d86-9111dd091f1d').
narrative_ontology:cs_reading_relation('cb9ba65e-7074-49a2-9d86-9111dd091f1d', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('cb9ba65e-7074-49a2-9d86-9111dd091f1d', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('cb9ba65e-7074-49a2-9d86-9111dd091f1d', foundational, no_mediating_governing_stratum).
narrative_ontology:cs_axiom_status(no_mediating_governing_stratum, holdable).
narrative_ontology:cs_axiom_grounding('cb9ba65e-7074-49a2-9d86-9111dd091f1d', no_mediating_governing_stratum, instrumental).
narrative_ontology:cs_axiom('cb9ba65e-7074-49a2-9d86-9111dd091f1d', secondary, imperative_mandate_and_immediate_recall).
narrative_ontology:cs_axiom_status(imperative_mandate_and_immediate_recall, holdable).
narrative_ontology:cs_axiom_grounding('cb9ba65e-7074-49a2-9d86-9111dd091f1d', imperative_mandate_and_immediate_recall, conventional).
narrative_ontology:cs_reference_frame('cb9ba65e-7074-49a2-9d86-9111dd091f1d', federated_assembly_sovereignty).
narrative_ontology:cs_drift_state('cb9ba65e-7074-49a2-9d86-9111dd091f1d', post_suppression_century, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cb9ba65e-7074-49a2-9d86-9111dd091f1d', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, rank_and_file_industrial_workers).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_peasant_communes).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, smashed_state_machine_doctrine).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, imperative_recall_delegation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federated workplace and neighborhood assemblies that deliberate, decide, and execute through recallable delegates. They administer production, distribution, and defense directly, absorb the surplus freed from ministries, party machines, and private owners, and carry the costs of assembly time, delegate rotation, and militia service. Dissolving the federation would hand them back to employer or ministry control, so they hold and run the structure at once.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary).

% Producers in mills, mines, docks, and rail yards who gain control over hiring, output, discipline, and the length of the working day through their shop-floor assemblies. They contribute delegate hours and take turns on administrative duty. Leaving the council fold mid-struggle means facing the old regime or the party machine alone and unarmed.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rank_and_file_industrial_workers, beneficiary,
    organized, biographical, constrained, national).

% Village communes federating land, grain, and militia decisions with the urban councils. They gain relief from landlord rents and state requisition but depend on the federation for tools and urban manufactures. Exit means returning to landlord arbitration or requisitioning armies.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_peasant_communes, beneficiary,
    organized, generational, constrained, regional).

% Career administrators of the overthrown state apparatus whose posts, salaries, and command prerogatives the council system abolishes outright. Their expertise is wanted as ordinary technical labor; their office is not. Their positional capital — rank, patronage networks, procedural mastery — is worthless outside the machine they staffed, and defection to counter-revolution carries mortal risk.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Professional revolutionaries and party cadres whose organizational identity is built on leading the class to power. The council principle assigns them the same standing as any other delegate group and dissolves their claim to a directing role. Career, self-concept, and world-historical mission are fused with party supremacy, so accepting assembly subordination registers as self-annihilation rather than as one policy preference among others.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    organized, generational, identity_locked, continental).

% Mensheviks, left-Socialist-Revolutionaries, anarchists, and independent socialists seeking council seats and press freedom as minority tendencies. The reading's ideal admits them as factions within the assemblies; wartime practice repeatedly narrowed the councils toward a single tendency, leaving them publishing, striking, or imprisoned at the margins.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, non_aligned_socialist_tendencies, excluded,
    organized, biographical, constrained, national).

% Theorists and historians in the Pannekoek-Korsch line and later councilist currents who reconstruct the Commune-to-soviets lineage, compare council episodes across countries, and assess whether each instance realized or betrayed the principle. They hold no votes, pay no levies, and bear no repression aimed at the assemblies themselves.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, council_communist_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production, distribution, and defense across workplaces and localities without either wage-labor markets or a standing state and party apparatus: federated assemblies with recallable, imperatively mandated delegates aggregate local knowledge, execute common decisions, and keep decision power at the point of production.
% TRANSFER_FUNCTION: Moves decision authority and surplus control from state ministries, party central committees, and private owners to the federated workplace assemblies; moves the offices, salaries, and command rents of the officialdom out of existence, releasing what they consumed to the producing collectives.
% ABSENT_VOICES: The non-aligned socialist tendencies and the displaced officialdom itself would object loudest; also the technical specialists whose cooperation the economy required but whose status distinctions the arrangement leveled. They sit outside the room as expellees, prisoners, or self-silenced witnesses under civil-war conditions; in the reading's own ideal they sit inside the assemblies as protected minority factions.
% DISAPPEARANCE_RATIONALE: If the council system vanished overnight, the collectives' coordination of production, distribution, and defense collapses back into whichever rival form is nearest — ministry, party committee, or employer — and the surplus currently retained by the producers is re-appropriated by whatever stratum re-forms. Every dependent arrangement (federated communes, recallable delegation, armed neighborhood defense) unravels with it.
% FOUNDING_PROBLEM: Built to solve the problem the Paris Commune posed and the 1905 and 1917 soviets confirmed: how can the working class exercise power directly after breaking the old regime without recreating a separate governing stratum that hardens into a new exploiting class?
% FOUNDING_PROBLEM_CORROBORATION: The problem's reality is corroborated from outside the benefiting parties: contemporary diplomatic dispatches and later academic labor history document both the Commune's fate and the 1905 St. Petersburg Soviet's coordination capacity, and the recurring spontaneous appearance of council-form bodies in crises (Turin 1920, Hungary 1956, Portugal 1974, Poland 1980) is attested by observers hostile to the reading. No party outside the beneficiary set attests that the council form solves the problem — corroboration extends to the problem, not to the solution's efficacy.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because recallable, imperatively mandated delegation leaves little standing apparatus through which extraction could accumulate; the residual reflects delegate overhead, transition-period requisition, and wartime pressure toward labor militarization. Suppression (0.48) is moderate: the arrangement must actively suppress the re-formation of bureaucratic and party command (that suppression IS its founding purpose) and defend itself by arms, while recallability structurally minimizes coercion among participants. Theater is low (0.15): assembly deliberation is load-bearing, not ceremonial. Accessibility collapse is low-moderate (0.35): rival forms persisted everywhere and reasserted themselves at every opportunity. Resistance is very high (0.88): the council form met the century's most sustained armed resistance — crushed in Russia 1921, Germany 1919, Hungary 1919 and 1956, Spain 1937-39, Poland 1980-81, Shanghai 1967. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope downstream. Fixing cost is prohibitive: every historical attempt to remove the arrangement from outside required civil-scale violence, and dismantling it from inside would require the assemblies to vote away their own coordination at the price of re-staffing a hierarchy. The measurement series run on one shared eight-point grid (1917-2025) tracing the cyclical pattern: each revolutionary wave revives the council form at low extraction and rising enforcement need, and each suppression event cuts the cycle short. The oscillation is driven by external crisis rhythm, not by intermittent reinforcement engineered by the constraint itself; endpoints were sampled at revival phases, so the series understates peak wartime extraction (1921) relative to the scalar's steady-state reading.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the worker and commune seats the arrangement computes rope-like: genuine coordination, low effective extraction, net benefit. From the bureaucrat seat it computes snare-like: total expropriation of position with trapped exit. From the party-official seat it is worse than extraction — identity annihilation, since the constraint denies the premise their selves are built on. The excluded tendencies experience a fourth version: a formally open structure that narrows under siege. One arrangement, four constraint-experiences; the divergence is computed from role, power, and exit data, not asserted.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the collectives, rank-and-file workers, and peasant communes receive decision authority and released surplus (d near the beneficiary end, amplified by their organized-but-constrained position). Victim declarations map to abolished positions: state bureaucrats (d near full-target, trapped — their positional capital is constraint-specific and worthless elsewhere) and vanguard party officials (near full-target, identity_locked — exit is unthinkable because it requires abandoning the fused cadre identity). The excluded tendencies sit near symmetric: intended participants bearing partial costs of wartime narrowing. The theorist seat is analytical and feeds no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Dismissing the arrangement as a dead utopian idea (piton) mistakes murder for atrophy — every historical instance was terminated by superior external force while its internal function remained live, which is why theater_ratio stays low and founding_problem_status reads contested rather than dead. Romanticizing it as pure rope ignores the real, concentrated costs the same structure imposes on the officialdom and the repeated wartime narrowing against minority tendencies. Tangled rope keeps both halves visible: the coordination function is genuine, the extraction is real but concentrated on a small defeated class rather than diffused over participants, and enforcement is constitutive, not incidental. The R5 mismatch consumer reads status=contested x verdict=world_rearranges: no zombie flag fires, correctly — the founding problem revives with every crisis that produces new councils.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the manifesto_revolutionary_method kernel — council, vanguard, or gradualist — actually secures durable working-class power without generating a new exploiting stratum?',
    'Comparative institutional analysis of all three readings'' historical instances under matched conditions; Russia 1917-1921 hosts council and vanguard forms in sequence over the same population and economy, providing a natural paired comparison.',
    'Reclassification pressure across the whole kernel family: if vanguard instances show lower lifetime extraction than council instances, this reading''s beneficiary declarations and low epsilon shift; if the reverse, the sibling files inherit the upward correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest among three readings of revolutionary method; this story is one branch of that contest.').

omega_variable(
    external_suppression_confound,
    'How much of the arrangement''s measured suppression is its own coercive requirement, and how much is suppression imposed on it by rival readings'' enforcement machinery?',
    'Compare council instances facing low external hostility (peacetime cooperatives, Poland 1980 before martial law) with besieged ones (Russia 1918-21, Catalonia 1936-37): if internal coercion tracks siege intensity, the scalar is externally inflated.',
    'If externally inflated, base suppression falls toward 0.3 and participant seats compute closer to rope; if intrinsic, the tangled_rope reading holds with higher effective extraction on all governed seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_suppression_confound, empirical, 'Separating the constraint''s own coercive force from the hostile environment it operated in.').

omega_variable(
    delegate_stratification_recurrence,
    'Does imperative mandate plus immediate recall permanently prevent bureaucratic stratification, or only delay it — Michels'' iron law of oligarchy applied to councils?',
    'Longitudinal study of the longest-lived council and cooperative federations for emergent delegate privilege, tenure creep, and information asymmetry between delegates and assemblies.',
    'If stratification recurs, epsilon trends upward over the lifecycle and mature instances drift toward snare; if it does not, the low epsilon is structurally stable and the anti-bureaucratic design claim is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegate_stratification_recurrence, empirical, 'Whether the anti-bureaucratic design holds against oligarchic drift in long-running instances.').

omega_variable(
    permanence_versus_transition_viability,
    'Is the council form viable as a permanent constitution, as this reading uniquely claims against both siblings, or only as a crisis-phase organ that peacetime complexity erodes?',
    'Conceptual analysis of peacetime coordination scale combined with the longest continuous council experiments; watch for assemblies ceding routine coordination to technical bodies that then acquire discretion.',
    'If transitional-only, the arrangement resembles a scaffold lacking its sunset clause and the reading owes an account of the handoff it denies needing; if permanent-viable, the current classification and low epsilon stand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permanence_versus_transition_viability, conceptual, 'The reading''s distinctive permanence claim tested against crisis-organ evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(mani_tr_t1921, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1921, 0.3).
narrative_ontology:measurement(mani_tr_t1936, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1936, 0.15).
narrative_ontology:measurement(mani_tr_t1956, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1956, 0.08).
narrative_ontology:measurement(mani_tr_t1968, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(mani_tr_t1980, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(mani_tr_t2011, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(mani_tr_t2025, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.22).
narrative_ontology:measurement(mani_be_t1921, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1921, 0.38).
narrative_ontology:measurement(mani_be_t1936, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1936, 0.3).
narrative_ontology:measurement(mani_be_t1956, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1956, 0.28).
narrative_ontology:measurement(mani_be_t1968, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1968, 0.26).
narrative_ontology:measurement(mani_be_t1980, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1980, 0.24).
narrative_ontology:measurement(mani_be_t2011, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 2011, 0.2).
narrative_ontology:measurement(mani_be_t2025, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.42).
narrative_ontology:measurement(mani_su_t1921, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1921, 0.78).
narrative_ontology:measurement(mani_su_t1936, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1936, 0.66).
narrative_ontology:measurement(mani_su_t1956, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1956, 0.58).
narrative_ontology:measurement(mani_su_t1968, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1968, 0.36).
narrative_ontology:measurement(mani_su_t1980, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(mani_su_t2011, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 2011, 0.34).
narrative_ontology:measurement(mani_su_t2025, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto's revolutionary method' decomposes into three structurally distinct constraints sharing one kernel. This reading (council) and the vanguard reading both operate through workers' councils historically — the vanguard reading's defining move was capturing the same soviet institutions this reading treats as sovereign — so the network edge runs both ways: this reading supplies the institutional substrate the vanguard reading captured, and the vanguard reading's success is the proximate cause of this reading's suppression-series peaks (1921). The gradualist reading shares neither institutions nor victims and connects only at the kernel level. Epsilon differs by construction: 0.25 here (low, within-councils), with the siblings authoring their own values over their own arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
