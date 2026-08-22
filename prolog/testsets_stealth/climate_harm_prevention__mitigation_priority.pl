% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The constraint is the legitimacy boundary of climate politics: what
 *   counts as a legitimate climate response. This story instantiates the
 *   mitigation_priority reading of the kernel climate_harm_prevention —
 *   legitimate response means emissions reduction first, pursued through
 *   technological transition, inside a growth framework. The arrangement
 *   coordinates a real global collective-action problem: it produced the
 *   Paris architecture, common accounting, and the investment expectations
 *   that drove renewable costs down. It simultaneously carries asymmetric
 *   extraction: the growth-compatibility premise shields present affluent
 *   consumption from structural change, the finance hierarchy directs the
 *   large majority of climate money to future-oriented technology while
 *   present-vulnerable populations' adaptation claims stay subordinated, and
 *   green industrial and financial rents accumulate on flows whose emissions
 *   value is contested. The boundary is actively maintained by delegitimizing
 *   the sibling readings — adaptation-first as fatalism, degrowth as
 *   impossibility. Future generations are the declared primary beneficiaries
 *   and the structurally weakest seat: they cannot collect, consent, revise,
 *   or exit, and any pledge-delivery gap lands entirely on them. KEY AGENTS
 *   (by structural relationship): future_generations (primary declared
 *   beneficiary, powerless/trapped); renewable_energy_industries (primary
 *   material beneficiary, organized/mobile); green_finance_sector (secondary
 *   beneficiary, institutional/arbitrage); climate_policy_institutions
 *   (agenda_setter, institutional/identity_locked); national_governments
 *   (agenda_setter + payer, institutional/constrained);
 *   carbon_intensive_industries (payer with capture channel,
 *   powerful/arbitrage); fossil_fuel_sector_workers (concentrated payers,
 *   moderate/trapped); present_climate_vulnerable_populations (payers via
 *   subordination, powerless/trapped); present_affluent_consumers
 *   (beneficiaries of the growth shield, moderate/mobile);
 *   degrowth_adaptation_advocates (excluded, moderate/trapped);
 *   climate_policy_analysts (observer, analytical/analytical). The sibling
 *   readings are separate constraints in this kernel's family; this story is
 *   epsilon-invariant for the mitigation-priority arrangement only.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.5).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Climate Legitimacy Framework").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'f9975b0d-3ea7-4f71-82b7-f4040b5288ce').
narrative_ontology:cs_kernel_codification('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', distributed).
narrative_ontology:cs_authority_grounding('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', distributed).
narrative_ontology:cs_reading_relation('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', foundational, growth_compatible_decarbonization).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', growth_compatible_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', foundational, future_harm_prevention_precedence).
narrative_ontology:cs_axiom_status(future_harm_prevention_precedence, holdable).
narrative_ontology:cs_axiom_grounding('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', future_harm_prevention_precedence, deontological).
narrative_ontology:cs_axiom('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', secondary, technology_led_transition_sufficiency).
narrative_ontology:cs_axiom_status(technology_led_transition_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', technology_led_transition_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', green_growth_decarbonization_baseline).
narrative_ontology:cs_drift_state('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', contemporary_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f9975b0d-3ea7-4f71-82b7-f4040b5288ce', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_finance_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_policy_institutions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, present_affluent_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_sector_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, national_governments).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, green_growth_decoupling_hypothesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, mitigation_cost_benefit_precaution_case).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, pledge_and_review_governance_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will inherit whatever climate results from present policy. The framework is justified in their name: they are the intended recipients of avoided warming. They cannot participate in, consent to, revise, or exit the arrangement, and they receive its protection only through present actors who discount their interests. Any gap between what the framework promises and what it delivers lands entirely on them, with no recourse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Manufacture and deploy the technologies the transition framework directs capital toward — renewables, storage, electrification. Subsidy regimes, mandates, and investment expectations created by the framework constitute their demand base. They can shift capital across markets and jurisdictions and have organized into an effective industrial lobby for the framework's continuation and expansion.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_industries, beneficiary,
    organized, biographical, mobile, global).

% Structures, manages, and fees the capital flows the framework directs: green bonds, carbon markets, ESG products, transition finance. It collects a toll on transition capital regardless of which technologies win, and can reallocate across asset classes if the framework's returns deteriorate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_finance_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Administer the framework: run the negotiation cycles, set agenda items, produce the assessment reports that define what counts as legitimate climate policy, and review national pledges. Their authority, continuity, and professional identity are constituted by the framework they administer; the apparatus has become its function. They can shape the framework's content but cannot abandon it without dissolving themselves.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Ratify and implement the framework: set national pledges, legislate transition policy, allocate climate finance. They answer to present electorates bearing transition costs, which pulls implementation short of pledges. They are bound by treaty commitments and international finance conditions they cannot cheaply exit.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, national_governments, payer).

% Face stranded-asset risk, regulatory pressure, and demand decline from the transition. At the same time, the framework's growth-compatibility and technology-neutral channels direct subsidies toward them — carbon capture, hydrogen, offsets — that extend asset lives and defer structural decline. Their capital can move, delay, and capture policy design, and their lobbying materially shapes how much of the transition is real deployment versus extended asset life.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, beneficiary).

% Bear the transition's concentrated costs: job loss, regional economic decline, skill obsolescence in communities built around carbon-intensive employment. Just-transition compensation is promised by the framework's politics and persistently underfunded. Skills, homes, and social ties are location-bound; exit means personal rupture.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_sector_workers, payer,
    moderate, biographical, trapped, regional).

% Suffer present climate harms — floods, heat, crop failure, displacement — now. The framework's finance hierarchy directs the large majority of climate money to future-oriented mitigation technology rather than to the resilience and adaptation their present survival requires. They have the least mobility of any affected population and the least voice in finance allocation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_climate_vulnerable_populations, payer,
    powerless, immediate, trapped, regional).

% The framework's growth-compatibility premise promises climate action without lifestyle change. They bear diffuse, modest costs — green premiums, efficiency standards — while the structural consumption changes a faster transition would require are kept off the agenda their political preferences define. Their costless-transition preference is what the framework's design satisfies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_affluent_consumers, beneficiary,
    moderate, immediate, mobile, global).

% Hold the sibling positions: adaptation-first responses centered on present vulnerability, and planned-consumption-contraction responses centered on sufficiency. They publish, organize, and testify, but sit outside agenda-setting: negotiation agendas, finance windows, and mainstream feasibility discourse are administered inside the growth-framework premise their positions contest. Exiting the climate policy conversation would abandon the populations they advocate for.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_adaptation_advocates, excluded,
    moderate, generational, trapped, global).

% Track pledge-delivery gaps, adaptation-mitigation finance ratios, decoupling evidence, and offset integrity. They can see the framework's coordination achievements and its shortfalls simultaneously and hold no stake in either sibling reading's victory.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, renewable_energy_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine global collective-action problem: provides shared temperature targets, a common accounting metric (CO2e, national pledge cycles), a legitimacy standard that lets governments impose present concentrated costs for diffuse future benefits, and stable investment expectations that contributed to renewable cost declines. It aligns roughly 195 parties on emissions reduction as the primary object of climate policy.
% TRANSFER_FUNCTION: Moves present resources — public climate finance, private investment, political attention, regulatory permission — from present consumption and carbon-intensive sectors toward mitigation technology and its financial intermediaries; simultaneously defers structural consumption change in the present and subordinates present climate-vulnerable populations' claims on adaptation resources behind mitigation flows.
% ABSENT_VOICES: Future generations — the framework's primary declared beneficiaries — are absent and cannot consent, object, or exit; their interests are represented by advocates with imperfect alignment and by discount rates they did not choose. Present climate-vulnerable populations are formally seated (loss-and-damage agenda items) but structurally subordinated in finance allocation. Degrowth and adaptation-first advocates hold live readings but are excluded from agenda-setting: negotiation agendas, finance windows, and mainstream feasibility discourse are administered inside the growth-framework premise their readings contest.
% DISAPPEARANCE_RATIONALE: Climate policy would fragment overnight: shared temperature targets, pledge cycles, carbon markets, and green investment expectations would lose their legitimacy anchor. The sibling readings would compete to fill the vacuum, and trillions in directed capital along with the entire institutional apparatus (negotiation system, assessment bodies, net-zero commitments) would need to reorganize around a different or absent legitimacy standard.
% FOUNDING_PROBLEM: Climate change is a collective-action problem whose costs fall on present, concentrated actors while benefits accrue diffusely in the future: without a shared legitimacy framework, no government can sustainably impose transition costs against present-bias and free-riding. The framework was built at Rio (1992), Kyoto (1997), and Paris (2015) to solve how to coordinate worldwide emissions reduction while remaining politically sustainable inside growth-dependent economies — hence the technological-transition, growth-compatible design.
% FOUNDING_PROBLEM_CORROBORATION: Physical climate science corroborates from outside the beneficiary set: observed warming of roughly 1.2-1.3C above preindustrial, remaining carbon budgets, and attribution studies all attest the founding coordination problem is live. Sibling-reading advocates — adaptation-first and degrowth — corroborate the problem's liveness from outside the benefiting parties while contesting the growth-compatible formulation. No corroborating source outside the beneficiary set attests that the growth-framework component itself is required for the response; that component is attested only by the framework's own beneficiaries and administrators.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the coordination is real, but the growth-compatibility component operates as a present-generation protection device — ambition is discounted to what remains compatible with continued growth, adaptation finance stays a small fraction of the total despite parity commitments, and intermediation rents accumulate on offset and transition flows of contested emissions value. Suppression 0.50: the boundary is enforced discursively and institutionally — feasibility discourse, funding gatekeeping, agenda control — rather than through state coercion of dissenters; alternatives remain speakable but not fundable or agenda-able. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and global scope, which raises verification difficulty for the target seats). Theater_ratio 0.42: pledge proliferation (net-zero targets without transition plans, offset-based neutrality claims) is a large and growing share of framework activity, though real deployment is substantial. Accessibility_collapse 0.40: the sibling readings remain visible and argued — they have not collapsed — but sit outside agenda-setting, so the practical alternative set is narrowed without being eliminated. Resistance 0.60: degrowth and climate-justice movements, adaptation advocates, and carbon-intensive sectors all contest the arrangement from different directions. The measurement series share one grid (t0=1992 Rio/UNFCCC era; t7~1999 post-Kyoto; t14~2006 CDM at scale; t21~2013 post-Copenhagen; t28~2020 Paris implementation and net-zero cascade; t34~2026 present), all points historically observed. Extractiveness and theater rise with the widening pledge-delivery gap and pledge proliferation; suppression_requirement rises because this story specifically tracks the enforcement machinery of the legitimacy boundary, which matured and hardened over the interval (net-zero norm enforcement, greenwashing regulation, hardening feasibility discourse) — an enforcement-intensification trajectory, hence its inclusion alongside the other two series. Receipt: the arrangement's directed flows land predominantly in renewable_energy_industries (the gain_flow seat), with green_finance_sector taking an intermediation toll and present_affluent_consumers receiving the deferred-sacrifice benefit; no single seat captures everything, but the largest material share is demonstrable. Fixing cost: moving the legitimacy boundary (admitting the sibling readings to agenda and finance) is prohibitive for the seats that could move it — electoral cost for governments, treaty renegotiation, and institutional self-dissolution for the administrative apparatus — relative to the benefit they would individually capture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From climate_policy_institutions and renewable_energy_industries the arrangement is coordination they built and staff — the legitimate core of climate politics. From fossil_fuel_sector_workers and present_climate_vulnerable_populations the same structure operates as a finance hierarchy that subordinates their claims. From degrowth_adaptation_advocates it is a foreclosure machine. Future generations occupy a seat no present actor can occupy: the declared beneficiary that cannot collect. national_governments sit split — agenda-setters internationally, payers domestically — which is why implementation lags pledge. The identity-lock on the institutional seat is organizational and professional: the apparatus has become its function, and its personnel's authority, careers, and self-conception are constituted by the framework; if that identity frame broke, the seat would compute far closer to the payers' position. Note also that the two powerless seats cannot coalition: one is absent entirely (future generations) and the other has voice without finance (present vulnerable populations), and their interests are represented by different advocate communities — mitigation-oriented and adaptation-oriented NGOs — that compete for the same finance windows.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: future_generations (declared primary beneficiary — though see the delivery-gap omega), renewable_energy_industries, green_finance_sector, climate_policy_institutions, and present_affluent_consumers, whose benefit is the growth-framework shield against structural consumption change. Victim declarations map to high directionality: fossil_fuel_sector_workers, present_climate_vulnerable_populations, and carbon_intensive_industries. Two corrections to the raw derivation: (1) carbon_intensive_industries appear in the victim set but hold a capture channel — the framework's technology-neutral channels (carbon capture, hydrogen, offsets) direct subsidies back to them and defer structural decline — so a directionality override sets the powerful atom to d=0.55 rather than the near-full-target value a pure victim reading would derive; they are the only powerful seat in the story, so the per-atom override is unambiguous. (2) future_generations combine a beneficiary declaration with powerlessness, trapped exit, and delivery-gap bearing; the override surface is keyed by power atom and cannot separate them from present_climate_vulnerable_populations, who are genuine full targets, so no override is authored for the powerless atom — the ambiguity is carried by the omega declared_beneficiary_delivery_gap instead. Scope: the arrangement operates at global scope, which amplifies effective extraction on the target seats through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the arrangement as pure coordination (its advocates' claim — a rope) would erase the extraction: the growth-compatibility premise is precisely the mechanism by which present consumption externalizes costs onto the future and onto the present vulnerable, and the finance subordination is measurable. Reading it as pure extraction cover (the degrowth claim — a snare) would erase the coordination: the emissions collective-action problem is real, the arrangement demonstrably moved capital and cost curves, and the sibling readings do not dispute that emissions must fall. Tangled rope holds both: genuine coordination function plus asymmetric extraction plus active enforcement. On mandatrophy: the founding problem — coordinating global emissions reduction against present-bias and free-riding — is live (global emissions have not peaked), so the arrangement's mandate has not outlived its function; what has degraded is the delivery-to-claim ratio, which surfaces as rising theater and extractiveness rather than mandate death. The R5 mismatch check (founding_problem_status=live x disappearance_verdict=world_rearranges) raises no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (mitigation_priority) of the kernel climate_harm_prevention; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the siblings are separate constraints (climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading) with their own epsilon, beneficiary/victim structure, and classification. The disagreement is located in two structural elements: the growth-compatibility premise (affirmed by this reading, denied by degrowth_reading) and the time-index of the primary beneficiary (future generations here; present vulnerable populations under adaptation_priority).',
    'If a sibling reading displaced this one as the operative legitimacy standard, the primary beneficiary seat shifts (future generations to present vulnerable populations under adaptation_priority; present affluent consumers become primary payers under degrowth_reading), the finance hierarchy inverts, and this constraint''s epsilon would be re-authored against a different referent rather than revised in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading this story instantiates, what the siblings would change, and where the disagreement sits.').

omega_variable(
    growth_compatibility_empirical_status,
    'Is decarbonization at the required rate actually compatible with continued economic growth, or does the growth-compatibility premise function as an ambition discount that the evidence cannot support?',
    'Decoupling evidence (absolute vs. relative, territorial vs. consumption-based accounting), energy-system modeling at 1.5C-consistent transition rates, and historical precedent for transition speed; the degrowth_reading is the live competing hypothesis on the same evidence base.',
    'If the premise fails empirically, this reading''s foundational axiom (growth_compatible_decarbonization) is overridden and the shielded-consumption component becomes the arrangement''s dominant function — the classification would drift from tangled_rope toward snare, with present_affluent_consumers as the capturing seat. If it holds, the coordination framing survives with extraction confined to the measurable rents and subordinations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_empirical_status, empirical, 'Whether the reading''s foundational empirical axiom survives the decoupling evidence.').

omega_variable(
    declared_beneficiary_delivery_gap,
    'Are future generations the actual beneficiaries of this arrangement or its actual victims — does the protection delivered match the protection the legitimacy claim promises?',
    'Compare the warming trajectory implied by implemented policy against the trajectory implied by the framework''s stated targets (pledge-delivery and ambition-gap accounting), and price the transition delay attributable specifically to the growth-compatibility premise.',
    'If the delivery gap is large, the declared primary beneficiary is structurally a partial target: their directionality sits well above the beneficiary end, the arrangement''s extractive component is larger than payer-side data alone suggests, and the coordination story covers a transfer from the future to the present. If the gap is small, the beneficiary declaration is accurate and the directionality structure is as declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declared_beneficiary_delivery_gap, empirical, 'Whether the declared primary beneficiary actually collects what the legitimacy claim promises.').

omega_variable(
    sibling_suppression_mechanism,
    'Is the suppression of the sibling readings (adaptation-first, degrowth) structural — funding gatekeeping, agenda control, finance-window design — or internalized — feasibility beliefs held by trained policy professionals that persist independent of any barrier?',
    'Track what happens to sufficiency and adaptation-first proposals at each institutional gate: are they rejected on funded criteria (structural) or never generated or proposed by insiders (internalized)? Natural experiments: jurisdictions that adopted adaptation-first or sufficiency policies, and the professional discourse that followed.',
    'If suppression is mostly structural, opening the agenda gates would revive the siblings quickly and the measured suppression overstates the arrangement''s durability. If internalized, the boundary persists after formal gates open — the effective suppression is higher than the structural measure shows and the framework outlives its enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_suppression_mechanism, empirical, 'Structural vs. internalized mechanism maintaining the legitimacy boundary against sibling readings.').

omega_variable(
    incumbent_capture_channel,
    'How large is the recapture channel through which carbon-intensive industries convert transition pressure into transition rents (carbon capture, hydrogen, offsets, delay), and does it exceed the transition costs they bear?',
    'Subsidy-flow accounting by recipient sector; stranded-asset write-down data; offset and carbon-capture credit integrity audits; lobbying expenditure against binding transition mandates.',
    'If recapture exceeds costs borne, carbon_intensive_industries are net beneficiaries despite their victim declaration — the arrangement''s costs land almost entirely on workers, the present vulnerable, and the future, strengthening the tangled_rope reading and the d=0.55 override for the powerful atom. If costs dominate, they are genuine targets and the arrangement''s costs are more broadly distributed across the payer set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_channel, empirical, 'Whether the arrangement''s most visible opponents are also among its rent collectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__mitigation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t7, climate_harm_prevention__mitigation_priority, theater_ratio, 7, 0.24).
narrative_ontology:measurement_basis(clim_tr_t7, observed).
narrative_ontology:measurement(clim_tr_t14, climate_harm_prevention__mitigation_priority, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(clim_tr_t14, observed).
narrative_ontology:measurement(clim_tr_t21, climate_harm_prevention__mitigation_priority, theater_ratio, 21, 0.33).
narrative_ontology:measurement_basis(clim_tr_t21, observed).
narrative_ontology:measurement(clim_tr_t28, climate_harm_prevention__mitigation_priority, theater_ratio, 28, 0.38).
narrative_ontology:measurement_basis(clim_tr_t28, observed).
narrative_ontology:measurement(clim_tr_t34, climate_harm_prevention__mitigation_priority, theater_ratio, 34, 0.42).
narrative_ontology:measurement_basis(clim_tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__mitigation_priority, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t7, climate_harm_prevention__mitigation_priority, base_extractiveness, 7, 0.4).
narrative_ontology:measurement_basis(clim_be_t7, observed).
narrative_ontology:measurement(clim_be_t14, climate_harm_prevention__mitigation_priority, base_extractiveness, 14, 0.45).
narrative_ontology:measurement_basis(clim_be_t14, observed).
narrative_ontology:measurement(clim_be_t21, climate_harm_prevention__mitigation_priority, base_extractiveness, 21, 0.5).
narrative_ontology:measurement_basis(clim_be_t21, observed).
narrative_ontology:measurement(clim_be_t28, climate_harm_prevention__mitigation_priority, base_extractiveness, 28, 0.54).
narrative_ontology:measurement_basis(clim_be_t28, observed).
narrative_ontology:measurement(clim_be_t34, climate_harm_prevention__mitigation_priority, base_extractiveness, 34, 0.58).
narrative_ontology:measurement_basis(clim_be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__mitigation_priority, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t7, climate_harm_prevention__mitigation_priority, suppression_requirement, 7, 0.33).
narrative_ontology:measurement_basis(clim_su_t7, observed).
narrative_ontology:measurement(clim_su_t14, climate_harm_prevention__mitigation_priority, suppression_requirement, 14, 0.38).
narrative_ontology:measurement_basis(clim_su_t14, observed).
narrative_ontology:measurement(clim_su_t21, climate_harm_prevention__mitigation_priority, suppression_requirement, 21, 0.42).
narrative_ontology:measurement_basis(clim_su_t21, observed).
narrative_ontology:measurement(clim_su_t28, climate_harm_prevention__mitigation_priority, suppression_requirement, 28, 0.46).
narrative_ontology:measurement_basis(clim_su_t28, observed).
narrative_ontology:measurement(clim_su_t34, climate_harm_prevention__mitigation_priority, suppression_requirement, 34, 0.5).
narrative_ontology:measurement_basis(clim_su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate climate response' decomposes into three structurally distinct readings of the kernel climate_harm_prevention — mitigation_priority (this story: future-harm prevention via growth-compatible technological transition), adaptation_priority (present-harm management via near-term resilience), and degrowth_reading (sufficiency via planned contraction). The readings have different beneficiary time-indices, different victim sets, and different epsilon; they are separate constraints linked here, not one constraint with a policy parameter. This reading is the upstream member: it currently holds the legitimacy high ground and structurally conditions the siblings' resource availability and agenda access, which is why its edges point at both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
