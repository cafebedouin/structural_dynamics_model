% ============================================================================
% CONSTRAINT STORY: imperial_court_kyoto_dormant_legitimacy
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_court_kyoto_dormant_legitimacy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imperial_court_kyoto_dormant_legitimacy
 *   human_readable: Imperial Court at Kyoto as Dormant-Activatable Legitimacy Container (Tokugawa Shogunate)
 *   domain: japanese_history/dormant_authority
 *
 * SUMMARY:
 *   The Imperial Court at Kyoto under the Tokugawa Shogunate (1603-1868)
 *   represents a distinct structural configuration: a formally supreme
 *   authority preserved in dormant state, with legitimacy kernel intact but
 *   operational role suspended. The bakufu required the court's investiture
 *   to authorize shogunal rule, yet simultaneously monopolized all
 *   substantive governance, taxation, military control, and foreign
 *   relations. The court survived on minimal stipend, performing elaborate
 *   rituals that affirmed regime legitimacy while exercising no authority.
 *   This arrangement persisted for 265 years through active enforcement by
 *   the bakufu — constant suppression of court attempts to expand authority,
 *   control over court succession through adoption and marriage approval,
 *   limitation of court access to regional daimyo, and careful calibration of
 *   ritual privileges that affirmed status without enabling power. The
 *   constraint demonstrates that nested authority systems retain memory of
 *   prior configurations and can reactivate dormant outer containers when
 *   operational legitimacy erodes. During the Boshin Restoration (1868), the
 *   preserved court became the rallying point for anti-bakufu forces, and the
 *   emperor was activated as the symbol of regime change — not because the
 *   court gained new power, but because the bakufu's operational legitimacy
 *   collapsed under pressure from foreign contact, inequality, and internal
 *   division. The court's preservation throughout the Tokugawa period
 *   functioned as a backup legitimacy container, held dormant but ready for
 *   activation.
 *
 * KEY AGENTS:
 *   - Tokugawa Bakufu: Primary beneficiary (institutional/arbitrage) — extracts legitimacy from court's investiture while suppressing court's operational authority
 *   - Imperial Court at Kyoto: Primary victim (institutional/trapped) — dependent on bakufu stipend; operationally suspended despite formal supremacy
 *   - Regional Daimyo (Tozama): Secondary agents (powerful/constrained) — recognize court's formal supremacy but subordinate to bakufu military and political control; see court as potential activation point for resistance
 *   - Anti-Bakufu Coalition (late Tokugawa): Secondary beneficiary-potential (organized/constrained) — court's preservation creates alternative legitimacy source for regime opposition
 *   - Imperial Legitimacy as Abstract Function: Victim (institutional/trapped) — the very act of preserving the court suspends its function; activation requires destruction of the preservation arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_court_kyoto_dormant_legitimacy, 0.38).
domain_priors:suppression_score(imperial_court_kyoto_dormant_legitimacy, 0.62).
domain_priors:theater_ratio(imperial_court_kyoto_dormant_legitimacy, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_court_kyoto_dormant_legitimacy, extractiveness, 0.38).
narrative_ontology:constraint_metric(imperial_court_kyoto_dormant_legitimacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imperial_court_kyoto_dormant_legitimacy, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_court_kyoto_dormant_legitimacy, tangled_rope).
narrative_ontology:human_readable(imperial_court_kyoto_dormant_legitimacy, "Imperial Court at Kyoto as Dormant-Activatable Legitimacy Container (Tokugawa Shogunate)").
narrative_ontology:topic_domain(imperial_court_kyoto_dormant_legitimacy, "japanese_history/dormant_authority").

domain_priors:requires_active_enforcement(imperial_court_kyoto_dormant_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_court_kyoto_dormant_legitimacy, tokugawa_bakufu).
narrative_ontology:constraint_beneficiary(imperial_court_kyoto_dormant_legitimacy, imperial_court_prestige_preservation).
narrative_ontology:constraint_victim(imperial_court_kyoto_dormant_legitimacy, imperial_operational_authority).
narrative_ontology:constraint_victim(imperial_court_kyoto_dormant_legitimacy, court_economic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPERIAL COURT (MOUNTAIN) — The court experiences its own legitimacy as a natural and continuous institution. Formal authority derives from mythic origins (continuous imperial line); investiture authority is inherent rather than delegated. The court perceives its role as suspended temporarily by circumstance, not fundamentally altered. From within this frame, the constraint on operational authority is external and circumstantial, not structural. The court's kernel (ritual authority, legitimacy transmission) remains intact and unchanged.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 2: TOKUGAWA BAKUFU (TANGLED ROPE) — The bakufu receives genuine coordination benefit from the court's preserved legitimacy: the shogun's investiture by the emperor provides authority backup and ritual continuity that stabilizes the regime. This is real coordination — the court's existence solves the bakufu's legitimacy problem. Simultaneously, the bakufu extracts significant benefit by controlling the court's economic resources, limiting its access to daimyo (regional lords), and monopolizing the court's investiture function. The court is constrained from both below (economic dependence) and above (bakufu control of access and resources). The coordination is asymmetric: the bakufu benefits from the court's legitimacy while actively suspending the court's political role.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL OPERATIONAL AUTHORITY (SNARE) — Viewed as an abstract institutional function rather than individual agents, imperial operational authority is trapped. The court cannot exit its dependent relationship with the bakufu without destroying the preservation of legitimacy — if the court attempted to exercise authority independently, the bakufu would shut it down entirely or eliminate it. Yet the court also cannot exit by dissolving itself — doing so would abandon the legitimacy kernel that might become valuable later. The operational authority is suspended indefinitely with no pathway to restoration without overthrowing the bakufu system entirely.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: COURT CEREMONIAL FUNCTIONS (PITON) — The elaborate imperial rituals, seasonal ceremonies, and investiture procedures are substantially performative from the bakufu's perspective. The ceremonies affirm legitimacy through ritual repetition while the bakufu controls all substantive governance and policy. The court's ritual role has degraded from actual authority-exercise to symbolic affirmation. Theater ratio (0.81) reflects this: most court activity is ceremonial rather than operational. Yet the ceremonies persist through institutional inertia because dismantling them would risk breaking the legitimacy container itself. The bakufu maintains the theater not because it functionally needs the ceremonies but because the ceremonies anchor the investiture authority that maintains regime stability.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-BAKUFU FORCES / RESTORATION MOVEMENT (ROPE) — From the perspective of agents who oppose bakufu rule (tozama daimyo, lower samurai, emerging merchant class), the preserved imperial court represents a pure coordination mechanism for alternative legitimacy. The court exists as a latent container that can be activated: a coalition opposing bakufu authority can invoke the emperor's formal supremacy to delegitimize the shogun and mobilize resistance. This perspective sees the constraint as Rope because the court's preservation solves a genuine coordination problem (providing an alternative legitimacy source) with no extractive mechanism targeting the coalition itself. The coalition experiences the constraint as beneficial rather than extractive.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD WITH SUNSET) — From a systems perspective, the Tokugawa settlement is a temporary stabilization mechanism with built-in sunset logic. The bakufu's power rests on suppressing regional military capacity (through castle destruction edicts and weapon control) and controlling the imperial legitimacy container. Both suppressions require continuous enforcement. As economic development (Edo urbanization, merchant class emergence) and technological change (firearms, shipbuilding) accumulate pressure, the bakufu's enforcement capacity against both samurai regionalism and technological diffusion declines. The preserved court becomes increasingly valuable as an alternative legitimacy source — not because the court itself becomes more powerful, but because the bakufu's operational legitimacy erodes. The constraint's sunset is contingent on external pressure accumulation: if foreign contact, technological diffusion, or internal inequality pressure reaches critical thresholds, the dormant outer container becomes the obvious activation site for regime change.
constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_court_kyoto_dormant_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_court_kyoto_dormant_legitimacy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(imperial_court_kyoto_dormant_legitimacy, TR),
    TR >= 0.70.

:- end_tests(imperial_court_kyoto_dormant_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The bakufu extracts genuine value from the court's suspended authority — the shogun's legitimacy is enhanced by imperial investiture, the bakufu controls the court's stipend and access to regional power. However, the extraction is constrained by the bakufu's need to preserve the court's legitimacy kernel intact. If the bakufu extracted more aggressively (executing emperors, dismantling ritual functions, claiming divine right directly), the court's legitimacy would degrade and the coordination function would collapse. The moderate extractiveness reflects the bakufu's dependence on the very institution it is suppressing. Suppression (0.62): High. The bakufu enforces court suspension through multiple mechanisms: economic dependence (minimal stipend), political isolation (control of court access to daimyo and merchants), personnel control (bakufu approval of imperial adoption and succession), and direct intervention (Ansei Purge, suppression of pro-imperial factions). Yet suppression cannot be absolute — the court must retain enough autonomy to maintain its legitimacy function, its ritual authority, and its succession continuity. The constraint is therefore high-suppression tangled_rope, not pure snare. Theater ratio (0.81): Very high. By the late Tokugawa period, court ceremonies are substantially performative. The elaborate investiture rituals, seasonal observances, and imperial pronouncements affirm legitimacy through repetition while the bakufu makes all substantive decisions. The increase in theater ratio over the interval (0.68→0.81) reflects accumulating degradation: as the bakufu's own legitimacy erodes (foreign pressure, inequality, lost face), the court ceremonies become more elaborate and more theatrical in compensation — the regime attempts to shore up legitimacy through ritual intensification. This is characteristic piton behavior: degraded primary function maintained through theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the court's self-perception (mountain — unchanging natural legitimacy) and the bakufu's operational reality (tangled_rope — the court's legitimacy is valuable precisely because it is suspended and available for reactivation). From the court's frame, the constraint is external circumstance; from the bakufu's frame, the constraint is an active, enforced relationship. The analytical observer perceives this gap as the space where activation becomes possible: the court's mountain-like stability is actually a preserved dormancy maintained by bakufu enforcement. When bakufu enforcement fails (foreign pressure, internal division, legitimacy erosion), the dormancy dissolves and the court transitions from victim (snare/mountain) to activation point (rope for opposition, alternative container for regime replacement). The gap between court self-perception and operational reality is the mechanism by which dormant-container activation functions — the court's legitimacy is preserved precisely by being unused, and becomes most valuable when the preserving regime loses standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Bakufu (institutional/arbitrage): Derives as beneficiary + arbitrage exit → low d (≈0.15) → negative χ. The bakufu benefits from the arrangement and can exit without existential loss. Court (institutional/trapped): Derives as victim + trapped exit → high d (≈0.92) → high χ ≈ 1.28. The court bears costs and cannot exit. Anti-bakufu coalition (organized/constrained): Derives as potential-beneficiary + constrained exit → moderate d (≈0.40). The coalition benefits from court preservation but faces resource constraints in mobilizing around it. Theater_ratio (0.81) dominates the piton classification — the ceremonial degradation is structural regardless of directional calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by clarifying that the preserved court is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where the coordination function (legitimacy provision) is itself extracted through suppression. The bakufu coordinates the regime's legitimacy problem by activating the court's investiture function, while simultaneously extracting benefit by controlling the court's economic and political role. The resolution is that both the coordination reading (rope) and the extraction reading (snare) are locally valid from their respective perspectives — the tangled_rope classification captures both. The mountain reading from the court's perspective is a false summit: the court perceives its legitimacy as natural and unchanging, but the structural data reveals that the legitimacy is actively preserved and contingently maintained by bakufu enforcement. The scaffold reading from the analytical perspective clarifies the sunset logic: the arrangement has built-in termination conditions (bakufu legitimacy erosion leading to activation threshold breach). The piton reading captures the degradation mechanism: ceremonial elaboration compensates for functional suspension, and theater_ratio increase signals the regime's attempt to shore up legitimacy through ritualization. No single type resolves the constraint; the entire presheaf (mountain+tangled_rope+snare+piton+rope+scaffold) is the accurate description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormant_activation_threshold,
    'At what point of bakufu delegitimation does the preserved imperial court transition from dormant to active outer container? What credibility or authority threshold must the bakufu cross downward?',
    'Historical analysis of late Tokugawa period: correlation between bakufu loss-of-face events (Opium Wars reverberations, unequal treaties, failed Ansei Purge, Boshin War positioning) and court invocations in anti-bakufu rhetoric. Measurement of court legitimacy invocation frequency and organizational coalition-building (Sonnō jōi movement) relative to bakufu authority crises.',
    'If threshold is low (minor loss of face sufficient): court activation becomes likely within 50-year window, enabling prediction of Boshin Restoration timing. If threshold is high (systemic collapse required): court activation is contingent and could fail, leaving bakufu in place longer or enabling alternative legitimacy sources to emerge. Maps to fourth resolution channel: dormant-container activation as distinct from snare escape, rope coordination, or scaffold sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dormant_activation_threshold, empirical, 'Threshold for dormant imperial court activation').

omega_variable(
    preservation_mechanism_durability,
    'Is the preservation of the imperial court a genuine institutional commitment by the bakufu, or an unstable equilibrium that would dissolve if tested? Could the bakufu have eliminated the court entirely without regime collapse?',
    'Counterfactual analysis: examination of bakufu actions when court legitimacy was directly challenged (e.g., Ansei Purge conflicts between bakufu and court factions). If bakufu immediately backed down from confrontation despite military superiority, this suggests genuine dependence on court legitimacy rather than mere inertia. Analysis of bakufu succession logistics — does shogunal investiture require specific court participation that bakufu cannot replicate without court cooperation?',
    'If bakufu genuinely depends on court legitimacy: the tangled_rope classification is correct — bakufu extraction is limited by the coordination function the court provides. If bakufu could eliminate the court but doesn''t: the preservation is strategic theater, and the constraint classifies more as snare (bakufu extracts value from court''s subordination while choosing not to destroy it). Different implications for activation threshold: genuine dependence lowers it; strategic preservation raises it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_mechanism_durability, empirical, 'Whether bakufu preservation of court reflects genuine legitimacy dependence').

omega_variable(
    economic_dependence_asymmetry,
    'Is the court''s economic dependence on bakufu stipend genuinely asymmetric, or did the bakufu depend on court cooperation enough that the court had de facto veto power over stipend disruption?',
    'Historical records of court-bakufu economic negotiations. Analysis of court reserves, alternative revenue sources (land rents, merchant patronage), and instances where court threatened reduction in ritual or investiture cooperation. If court was able to extract concessions or maintain budgets despite bakufu authority, this indicates constrained rather than trapped exit options.',
    'If asymmetric (bakufu controls stipend completely): court is trapped, victims perspective (snare) is accurate. If court has veto leverage through legitimacy dependence: court is constrained rather than trapped, and the power relationship is more symmetric than tangled_rope from bakufu perspective suggests. Affects directionality computation for institutional power atoms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_asymmetry, empirical, 'Asymmetry of court economic dependence on bakufu').

omega_variable(
    cosmetic_vs_structural_legitimacy,
    'Is the bakufu''s need for court investiture a genuine structural requirement (something bakufu regime cannot function without), or a cosmetic legitimacy enhancer that bakufu leadership believes necessary but could survive without?',
    'Analysis of bakufu propaganda and founding ideology vs actual governance mechanics. If bakufu daimyo (regional lords) required imperial sanction to obey shogun, this is structural. If bakufu could enforce compliance through military and economic power alone, investiture is cosmetic. Evidence: late-Tokugawa bakufu attempts to rule without court consultation (failed attempts suggest structure; successful rule without consultation suggests cosmetic).',
    'If structural: court becomes increasingly valuable over time as bakufu loses military advantage relative to tozama and emerging coalitions. If cosmetic: court activation is possible but not inevitable — bakufu could be replaced by alternative structures (merchant oligarchy, confederal daimyo council) that don''t require imperial legitimation. Affects sunset analysis and activation threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmetic_vs_structural_legitimacy, conceptual, 'Whether court legitimacy is structurally necessary or cosmetically enhancing').

omega_variable(
    fourth_resolution_channel_validity,
    'Is dormant-container activation a distinct resolution mechanism from snare escape, rope coordination success, or scaffold sunset? Or is it subsumable into existing categories?',
    'Comparative analysis with other constraint stories involving nested authority systems with dormant outer containers. Identification of instances where the outer container is preserved specifically to enable reactivation rather than to support ongoing coordination. Contrast with constraints where the outer container is either dissolved (escape) or actively used for coordination (rope) or intentionally expired (scaffold).',
    'If dormant-activation is distinct: the framework requires a fourth resolution channel and explicit modeling of activation thresholds as alternative pathway to mountain/rope/snare/scaffold. If subsumable: the constraint is a variant of mountain (court''s natural legitimacy) + tangled_rope (bakufu extraction), and activation is a reclassification event (mountain→rope transition when court is reactivated) rather than a distinct mechanism. Affects how the framework models nested authority and regime transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourth_resolution_channel_validity, conceptual, 'Whether dormant-container activation is a distinct framework mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_court_kyoto_dormant_legitimacy, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperialkyoto_tr_t0, imperial_court_kyoto_dormant_legitimacy, theater_ratio, 0, 0.68).
narrative_ontology:measurement(imperialkyoto_tr_t20, imperial_court_kyoto_dormant_legitimacy, theater_ratio, 20, 0.74).
narrative_ontology:measurement(imperialkyoto_tr_t40, imperial_court_kyoto_dormant_legitimacy, theater_ratio, 40, 0.8).
narrative_ontology:measurement(imperialkyoto_tr_t60, imperial_court_kyoto_dormant_legitimacy, theater_ratio, 60, 0.81).
narrative_ontology:measurement(imperialkyoto_tr_t80, imperial_court_kyoto_dormant_legitimacy, theater_ratio, 80, 0.81).

% Extraction over time
narrative_ontology:measurement(imperialkyoto_be_t0, imperial_court_kyoto_dormant_legitimacy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(imperialkyoto_be_t20, imperial_court_kyoto_dormant_legitimacy, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(imperialkyoto_be_t40, imperial_court_kyoto_dormant_legitimacy, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(imperialkyoto_be_t60, imperial_court_kyoto_dormant_legitimacy, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(imperialkyoto_be_t80, imperial_court_kyoto_dormant_legitimacy, base_extractiveness, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_court_kyoto_dormant_legitimacy, identity_coordination).
narrative_ontology:affects_constraint(imperial_court_kyoto_dormant_legitimacy, sonno_joi_movement_activation).
narrative_ontology:affects_constraint(imperial_court_kyoto_dormant_legitimacy, bakufu_legitimacy_erosion).

% DUAL FORMULATION NOTE:
% The imperial court constraint decomposes into two related stories: (1) Imperial Court Dormant Legitimacy (this story, ε=0.38, tangled_rope) — the bakufu's active suppression of court authority while preserving court legitimacy; (2) Sonnō Jōi Movement Activation (downstream, ε=0.58, snare) — the late-Tokugawa coalition's mobilization around the preserved imperial court as resistance legitimacy source. The second story traces how dormant-container activation transitions from potential to actual, and how the court's victims-perspective (snare) becomes the resistance-coalition's rope. ε increases between stories because the activation process introduces new extraction mechanisms (the Boshin War, restructuring costs, new state formation) not present during dormancy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
