% ============================================================================
% CONSTRAINT STORY: shame_attribution_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shame_attribution_mechanism, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shame_attribution_mechanism
 *   human_readable: Shame Attribution Mechanism
 *   domain: social/psychological/institutional
 *
 * SUMMARY:
 *   The shame attribution mechanism is a social coordination apparatus that
 *   marks individuals as norm-violators, enforcing behavioral alignment
 *   through reputational cost. The constraint exhibits structural
 *   characteristics of tangled coordination-extraction: it solves genuine
 *   collective problems (norm enforcement, trustworthiness signaling) while
 *   simultaneously enabling asymmetric power leverage (concentrated control
 *   over attribution, irreversible marking of targeted agents). The mechanism
 *   operates across interpersonal, institutional, and civilizational scales.
 *   Modern shame attribution is increasingly performative (theater ratio
 *   0.68): public scandal cycles provide symbolic moral catharsis while
 *   behavioral correction and rehabilitation outcomes remain minimal. The
 *   mechanism's extractiveness has risen over the measurement interval (0.38
 *   to 0.58) as digital platforms amplify shame reach and permanence, while
 *   institutions have simultaneously lost functional dependence on shame for
 *   coordination, replacing it with algorithmic norm enforcement. Suppression
 *   is high (0.65) because shame operates through internalized fear and
 *   reputational vulnerability—the target cannot exit by rational choice
 *   alone; they must rebuild identity and trust in alternative communities.
 *
 * KEY AGENTS:
 *   - Shamed Agents: Primary victims (powerless/trapped) — face irreversible reputation marking, public status loss, and biographical disruption with no structural exit pathway
 *   - Status Enforcers: Primary beneficiaries (institutional/arbitrage) — cultural authorities, media platforms, social institutions that consolidate norm-setting power and reputation arbitrage through selective shaming
 *   - Reputation Commons: Mixed actor (moderate/constrained) — the shared understanding of trustworthiness and status allocation that coordination mechanism serves but also degrades through systematic bias
 *   - Reform Coalition: Organized agents (organized/mobile) — victims' advocates, restorative justice frameworks, alternative trust systems building sunset pathways through decentralized reputation
 *   - Moral Theater Institution: Institutional actor (institutional/arbitrage) — scandal cycles, public confession rituals, redemption narratives that persist through inertia despite degraded coordination function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing culturally contingent shame attribution as immutable law of human social life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shame_attribution_mechanism, 0.58).
domain_priors:suppression_score(shame_attribution_mechanism, 0.65).
domain_priors:theater_ratio(shame_attribution_mechanism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shame_attribution_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(shame_attribution_mechanism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shame_attribution_mechanism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shame_attribution_mechanism, tangled_rope).
narrative_ontology:human_readable(shame_attribution_mechanism, "Shame Attribution Mechanism").
narrative_ontology:topic_domain(shame_attribution_mechanism, "social/psychological/institutional").

domain_priors:requires_active_enforcement(shame_attribution_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shame_attribution_mechanism, status_enforcers).
narrative_ontology:constraint_beneficiary(shame_attribution_mechanism, norm_arbiters).
narrative_ontology:constraint_victim(shame_attribution_mechanism, shamed_agents).
narrative_ontology:constraint_victim(shame_attribution_mechanism, reputation_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The shamed agent faces maximum extraction with no structural exit. Shame attribution is irreversible at biographical timescale; identity is publicly marked. No alternative community or reputation pathway available. Bears full cost of the attribution mechanism.
constraint_indexing:constraint_classification(shame_attribution_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Status enforcers (cultural authorities, media platforms, social institutions) benefit from the shame mechanism by consolidating norm-setting power. They experience the constraint as pure coordination—enforcing shared standards of acceptability. High exit optionality through reputation arbitrage: they can signal virtue by shaming or strategically forgive, extracting value from both.
constraint_indexing:constraint_classification(shame_attribution_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The reputation commons (shared understanding of trustworthiness, status allocation, norm enforcement) experiences shame attribution as mixed coordination and extraction. Genuine coordination function: shame aligns incentives and enforces behavioral norms. But asymmetric extraction: the mechanism disproportionately marks certain groups while exempting others, creating systematic reputation inequality. Constrained exit: the commons cannot abandon reputation entirely, but faces high costs to reform attribution rules.
constraint_indexing:constraint_classification(shame_attribution_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Reform coalitions (victims' advocates, restorative justice frameworks, alternative reputation systems) see shame attribution as a temporary mechanism with a sunset clause. Their sunset logic: as trust-network technology and distributed reputation systems mature, centralized shame attribution loses power. Alternative pathways (apology platforms, reputation redemption, community accountability) offer lower-theater verification of character change. Sunset estimated at 15-25 years as digital trust infrastructure matures.
constraint_indexing:constraint_classification(shame_attribution_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% The moral theater institution (scandal cycles, public confession rituals, redemption narratives) persists through institutional inertia despite degraded function. Modern shame attribution is largely performative: public condemnation provides symbolic catharsis without genuine behavioral correction or rehabilitation. Theater ratio (0.68) reflects that the ritual of shame (public judgment, virtue signaling) has replaced the function of shame (incentive alignment). The institution maintains itself through narrative repetition, not through effectiveness.
constraint_indexing:constraint_classification(shame_attribution_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal perspective, some form of status differentiation and reputational marking is inherent to group coordination: humans necessarily track trustworthiness and signal norm compliance. Shame is viewed as an immutable mechanism of social life—unmodifiable natural law of human society. However, this perspective risks naturalizing the specific institutional form (concentrated shame attribution, irreversibility, status asymmetry) rather than the general function (reputation tracking). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(shame_attribution_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shame_attribution_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shame_attribution_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shame_attribution_mechanism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shame_attribution_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shame_attribution_mechanism, TR),
    TR >= 0.70.

:- end_tests(shame_attribution_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint concentrates reputational power asymmetrically—status enforcers control attribution, shamed agents bear full cost. But the extraction is partial rather than total because genuine coordination function exists: shame does enforce norms and sustain trust signaling. The value reflects that the mechanism solves real problems while extracting value from the solution. Suppression (0.65): High. Shame operates through internalized vulnerability and fear of public marking. Structural barriers include identity lock (reputation is constitutive of self), platform permanence (digital shame is irreversible), and power asymmetry (targets cannot effectively dispute attributions). Exit is not impossible but prohibitively costly—requires identity reformation and community rebuilding. Theater ratio (0.68): High and rising. Modern shame attribution is substantially performative: moral panic cycles, virtue signaling through public condemnation, and redemption theater have replaced genuine behavioral correction. The rising trajectory (0.42→0.68) reflects that institutional effectiveness has declined while symbolic function has intensified—institutions maintain the ritual because it provides catharsis, not because it works.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's classification ranges across all six types, revealing fundamental disagreement about shame's function. The status enforcer sees pure coordination (rope)—they are solving norm enforcement problems. The reform coalition sees a temporary problem with sunset (scaffold)—distributed reputation systems will replace centralized attribution. The moral theater institution sees its own degraded ritual (piton)—the scandal cycle persists through institutional habit rather than effectiveness. The reputation commons sees mixed coordination and extraction (tangled rope)—genuine norm enforcement alongside systematic power bias. The shamed agent sees pure extraction (snare)—irreversible reputational capture with no path to recovery. The civilizational observer risks seeing natural law (mountain)—status differentiation is inherent to human groups—but this naturalizes a contingent institutional form. The perspectival gap reveals that shame attribution's classification depends entirely on the observer's structural position and power to exit the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's relationship to the extraction flow. Status enforcers with arbitrage exit (alternative reputation pathways, selective forgiveness, redemption narratives) experience low d—they can escape the mechanism's constraints. Shamed agents with trapped exit experience high d—reputation loss is irreversible at biographical scale and no alternative status pathways exist. The reputation commons with constrained exit experiences moderate d—they cannot abandon reputation entirely but can organize toward alternative systems. Organized reform agents with mobile exit experience lower d—they can mobilize toward structural change. The institutional theater maintains itself through inertia rather than active beneficiary enforcement, yielding piton classification despite institutional power. The analytical observer's canonical d value (0.73) is overridden by the risk that this perspective naturalizes contingency: the baseline assumption that human societies always mark reputation should not anchor the classification toward mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The shame attribution mechanism resolves the mandatrophy by revealing that the six types represent genuinely different structural positions within a single mechanism. The question is not 'which type is correct?' but 'what does each agent's experience of the constraint actually permit them to see?' The status enforcer's rope is their lived experience of coordination utility. The shamed agent's snare is their lived experience of irreversible capture. The reform coalition's scaffold is not aspirational but structural: alternative reputation systems are actually being built. The theater institution's piton is verifiable: compare scandal cycle outcomes to behavioral correction metrics and see that the ritual continues despite degraded function. The reputation commons' tangled rope is empirically measurable: genuine norm enforcement coexists with systematic bias in who gets shamed. The analytical observer's risk of false summit is the framework's diagnostic test: if you find yourself wanting to call something a mountain because 'it's just how humans are,' check whether you're naturalizing contingent institutions rather than identifying immutable limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shame_irreversibility_threshold,
    'What determines whether shameful attribution is functionally irreversible at biographical timescale?',
    'Longitudinal tracking of reputation recovery trajectories; measurement of forgetting curves in digital memory; comparison of redemption success rates across disclosure and anonymity conditions.',
    'If irreversibility is structural (true trapped exit): snare classification confirmed. If reversibility is possible through deliberate path (constrained exit): reclassify as tangled_rope from shamed agent perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shame_irreversibility_threshold, empirical, 'Functional irreversibility of shame attribution over biographical timescale').

omega_variable(
    attribution_asymmetry_mechanisms,
    'Are asymmetries in shame attribution (who gets shamed, whose shaming sticks) driven by structural power differentials or by genuinely different norm violations?',
    'Case-comparative analysis of identical behavioral violations; measurement of status, identity, and power variables predicting shame attribution success; randomized trials of identical transgressions attributed to different agent types.',
    'If power-driven: shame mechanism is extractive weapon targeting powerless (snare confirmed). If norm-violation-driven: shame is coordination mechanism (rope/tangled_rope). If mixed: tangled_rope confirmed with evidence of systematic bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_asymmetry_mechanisms, empirical, 'Attribution asymmetries: power-driven vs norm-violation-driven mechanisms').

omega_variable(
    distributed_reputation_systems_viability,
    'Can decentralized reputation systems (blockchain, federated trust networks, privacy-preserving credentials) actually replace centralized shame attribution as coordination mechanisms?',
    'Empirical implementation of distributed systems (e.g., decentralized identity, attestation networks); measurement of coordination effectiveness and norm-enforcement capacity relative to centralized shame mechanisms; identification of failure modes in decentralized designs.',
    'If viable: scaffold sunset is structural, reformation is plausible. If not viable: shame mechanism will persist despite reform efforts; reclassify as piton with longer theater maintenance window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_reputation_systems_viability, empirical, 'Technical viability of decentralized reputation systems as replacement mechanisms').

omega_variable(
    shame_versus_guilt_functional_distinction,
    'Does shame (other-directed attribution) serve fundamentally different coordination functions than guilt (self-directed accountability), or are they interchangeable reputation mechanisms?',
    'Cross-cultural analysis of guilt-based vs shame-based accountability systems; measurement of norm compliance rates and behavioral correction under each mechanism; analysis of resilience to power asymmetry.',
    'If shame is specialized tool for power maintenance: shame mechanism is extractive (snare). If shame and guilt are functionally equivalent: either can coordinate effectively; choice of shame reveals institutional extraction preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shame_versus_guilt_functional_distinction, conceptual, 'Functional distinction between shame-based and guilt-based accountability').

omega_variable(
    collective_action_problem_in_shame_attribution,
    'Does shame attribution solve or create collective action problems—does it enable coordination or prevent it through fear of attribution?',
    'Measurement of norm compliance and voluntary cooperation under public shame threat vs private accountability; analysis of behavioral inhibition in high-shame environments; comparison of collective problem-solving capacity.',
    'If solving coordination problem: tangled_rope confirmed. If creating inhibition: classification shifts toward snare with reduced coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_problem_in_shame_attribution, empirical, 'Shame attribution as solution vs creation of collective action problems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shame_attribution_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sham_tr_t0, shame_attribution_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sham_tr_t10, shame_attribution_mechanism, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sham_tr_t20, shame_attribution_mechanism, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(sham_be_t0, shame_attribution_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sham_be_t10, shame_attribution_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sham_be_t20, shame_attribution_mechanism, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shame_attribution_mechanism, identity_coordination).
narrative_ontology:affects_constraint(shame_attribution_mechanism, reputation_commons_pollution).
narrative_ontology:affects_constraint(shame_attribution_mechanism, status_enforcement_asymmetry).
narrative_ontology:affects_constraint(shame_attribution_mechanism, moral_panic_amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shame_attribution_mechanism, analytical, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
