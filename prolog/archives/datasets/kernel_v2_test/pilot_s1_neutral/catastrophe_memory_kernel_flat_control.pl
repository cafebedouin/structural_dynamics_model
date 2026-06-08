% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_kernel_flat_control
 *   human_readable: Ritual Catastrophe-Commemoration as Collective Memory Transmission Across Diaspora
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual catastrophe-commemoration — the ceremonial, repeated enactment of
 *   collective trauma and its remembrance across generations — functions as a
 *   binding mechanism in diaspora communities separated by geography and time
 *   from the original catastrophic event. The Jewish practice of Passover
 *   (commemorating the Egyptian exodus across ~3,500 years), Tisha B'Av
 *   (destruction of temples across ~2,000 years), and Yom HaShoah (Holocaust
 *   remembrance, ~80 years) exemplify this structure. The constraint exhibits
 *   a fundamental structural tension: the ritual genuinely solves a
 *   collective-action problem (how do dispersed, temporally distant
 *   communities maintain shared identity and historical memory?) while
 *   simultaneously extracting conformity, time, emotional labor, and
 *   narrative control from participants. The constraint's extractiveness is
 *   moderate (0.35) because the coordination function is real and meaningful
 *   to many participants, but the extraction is also real — measured in
 *   identity-locking, suppressed historical nuance, enforced participation,
 *   and institutional authority maintenance. Theater ratio (0.58) reflects
 *   that as temporal distance from the original catastrophe increases, the
 *   functional trauma-processing component diminishes while the performative
 *   identity-maintenance and institutional-legitimation components increase.
 *   The measurement trajectory shows rising theater (0.35 → 0.58 → 0.64) as
 *   direct experience of the catastrophe recedes into historical distance,
 *   then modest stabilization or decline (0.64 → 0.58) as modernity
 *   introduces competing memory media (documentation, institutional archives,
 *   secular education). This is a diagnostic exemplar of mandatrophy: the
 *   ritual was created to solve a real problem (collective trauma
 *   transmission across diaspora), but as that problem's character changed
 *   (from living memory to historical knowledge), the constraint persisted
 *   through institutional inertia even as its functional necessity
 *   diminished. The constraint resists mandatrophy resolution because it
 *   simultaneously maintains genuine coordination and enforces extraction —
 *   no single agent perceives it as entirely dysfunctional, and institutional
 *   beneficiaries (religious authority structures) have strong incentives to
 *   maintain the arrangement.
 *
 * KEY AGENTS:
 *   - Diaspora Child: Powerless/identity-locked (global scope) — bears maximum extraction through identity fusion with ritual participation; cannot exit without experiencing self-dissolution
 *   - Community Participant: Moderate/constrained (regional scope) — faces social cost of non-participation; both benefits from cohesion and pays through conformity and labor
 *   - Religious Institution / Rabbinical Authority: Institutional/arbitrage (global scope) — primary beneficiary; maintains interpretive authority and legitimation through ritual leadership; has exit options but chooses institutional perpetuation
 *   - Ritual Leader / Ceremonial Practitioner: Institutional/arbitrage (regional scope) — maintains ceremonial structure; recognizes increasing performative content; sees own role as theater maintenance
 *   - Secular Diaspora Cohort: Organized/mobile (national scope) — advocates for transition to non-ritual memory mechanisms (archives, education, secular memorials); has exited ritual structures and maintains memory through alternative channels
 *   - Analytical Observer: Analytical/analytical (universal scope) — risks naturalizing contingent institutional arrangements as immutable features of human collective psychology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel_flat_control, 0.35).
domain_priors:suppression_score(catastrophe_memory_kernel_flat_control, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel_flat_control, "Ritual Catastrophe-Commemoration as Collective Memory Transmission Across Diaspora").
narrative_ontology:topic_domain(catastrophe_memory_kernel_flat_control, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_memory_kernel_flat_control, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, community_identity_maintenance).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, intergenerational_authority_structure).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, diaspora_cohesion_mechanism).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, individual_autonomy_in_remembering).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, historical_nuance_suppression).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, competing_narratives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA CHILD (SNARE) — Identity-locked through ritual participation from birth. The child internalizes catastrophe memory as constitutive of self-identity; cannot imagine themselves outside the commemorative cycle without experiencing erasure of collective identity. Structural barriers to exit are psychological/identity-based rather than material — the child has geographic mobility but cannot exercise it without becoming a different person. Experiences maximum extraction: the ritual system captures attention, time, emotional labor, and conformity of memory-framing across a lifetime. No perceived benefit — the constraint appears as natural obligation.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIASPORA COMMUNITY MEMBER (TANGLED ROPE) — Constrained by social cost of non-participation and relational stakes in group cohesion. Genuine coordination function exists: the ritual solves the collective-action problem of maintaining cultural continuity across geographic dispersal and temporal distance. Simultaneously, the participant bears extraction: enforced emotional labor, time commitment, conformity requirements. Mixed experience — both coordinator and payer through the same mechanism. Has some agency (can propose ritual variations, lead ceremonies) but faces real reputational cost for deviation.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTION (ROPE) — Institutional beneficiary with arbitrage options. Benefits from ritual perpetuation through authority legitimation, resource concentration, transmission of interpretive authority across generations. Experiences the constraint as pure coordination: the institution is solving the genuine problem of maintaining diaspora cohesion and cultural identity continuity. Has exit options at institutional scale (could decline to lead or authorize rituals; could redefine memory transmission mechanisms). Organized coordination function — no perceived extraction, only the coordination benefit of a stable, reproducible mechanism for binding dispersed populations.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RITUALIST / CEREMONY LEADER (PITON) — Maintains the ceremonial structure with diminishing functional content. The leader may recognize that the ritual's original trauma-processing function has atrophied (the catastrophe is now 2000+ years distant; participants have no direct experience) yet the ritual persists through institutional inertia and theatrical maintenance of meaning. The ceremony leader sees their role as mostly performative — creating the appearance of continuity and authenticity rather than enabling genuine collective processing of remembered trauma. Theater ratio (0.58) reflects this degradation: substantial performative content (symbolic reenactment, aesthetic preservation, identity theater) alongside diminished functional trauma-processing.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: SECULAR DIASPORA COHORT (SCAFFOLD) — Organized agents (secular historians, archivists, educational institutions, historical societies) see ritual commemoration as a temporary transitional mechanism for memory transmission. This perspective acknowledges the historical necessity of ritual during pre-literacy and limited-literacy periods, when oral ceremonial transmission was the primary channel for historical continuity. But modern documentary, archival, and educational infrastructure can carry the memory function with lower extraction and higher historical nuance. The scaffold view is that institutional ritual should sunset into historical documentation and secular education as viable alternatives mature. Mobile exit options — these agents can and do choose to transmit history through non-ritual channels.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, human communities invariably encode collective trauma into ritual structures — this appears as an unchangeable feature of how humans maintain group identity and transmit historical memory across generations. Ritual catastrophe-commemoration is the natural solution to the binding problem: how do you hold a geographically dispersed community together when the original trauma is no longer lived experience? Ritual appears as a natural law of collective psychology and group survival. However, this perspective risks naturalizing what is structurally a contingent institutional arrangement maintained through active enforcement and identity-locking mechanisms.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate-low. The ritual genuinely solves a coordination problem of diaspora cohesion and cross-generational identity transmission that has few obvious substitutes at scale. This legitimate coordination function prevents the constraint from being classified as pure extraction (snare-level 0.65+). However, extraction IS present in the conformity enforcement, narrative control, institutional benefit, and identity-locking mechanisms. The value reflects mixed but real coordination alongside measurable extraction. Suppression (0.48): Moderate. Significant barriers to exit include social costs (reputation damage, community rejection), psychological barriers (identity fusion), and institutional enforcement (ritual leadership maintains compliance through social pressure and institutional authority). However, suppression is not total — individuals do exit or reduce participation, and alternative memory mechanisms do exist. Theater ratio (0.58): Moderate-high. The ritual's functional content (trauma processing, identity construction, intergenerational bonding) still exists, but substantial performative content has accumulated: ceremonial elaboration, aesthetic preservation, institutional legitimation theater, and identity theater that maintains collective identity rather than processing collective trauma. As temporal distance from the original catastrophe increases (from immediate lived experience to historical narrative), the ratio shifts toward performance. The 2000-year measurement reflects contemporary state: the ritual is substantially theatrical (Yom HaShoah is 80 years from Holocaust; Passover is 3,500 years from putative exodus; Tisha B'Av is 1,950+ years from temple destruction). The piton classification at the ritualist perspective captures this degradation: the ceremony leader maintains the form (theater) while recognizing the atrophied function (genuine collective trauma processing is no longer the primary mechanism — identity maintenance and institutional authority have become the effective drivers).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence between powerless identity-locked participants and institutional beneficiaries. The diaspora child perceives the constraint as an inescapable snare — maximum extraction, no perceived coordination benefit, identity-locked into participation. The community participant perceives tangled rope — genuine coordination function alongside real but bearable extraction. The religious institution perceives pure rope — coordination mechanism, no experienced extraction, legitimate authority maintenance. The ritualist perceives piton — degraded function maintained as theater. The secular cohort perceives scaffold — temporary mechanism being superseded by better alternatives (education, archives, secular memorials) with a plausible sunset. The analytical observer risks perceiving mountain — naturalizing as immutable law what is contingent institutional arrangement. This range (snare → rope → piton → scaffold → mountain) across six perspectives reveals the constraint's true character: a hybrid coordination-extraction mechanism whose classification depends entirely on structural position. The gap between powerless and institutional perspectives (snare vs. rope) is the largest: the same ritual system appears as absolute extraction to participants with no exit, and pure coordination to leaders with arbitrage options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to the extraction flow and their exit capacity. The diaspora child has high d (0.85+): trapped by identity-locking mechanisms despite geographic mobility (no structural barriers but psychological/identity barriers function equivalently), classified as victim by victim-status declaration. The community participant has moderate d (0.50-0.60): constrained exit (social cost of non-participation) and mixed beneficiary/victim status — both coordinator and payer. The religious institution has low d (0.15-0.25): arbitrage-grade exit options and primary beneficiary status. The ritualist has low d (0.20-0.30): institutional actor with arbitrage options, recognizes degradation but maintains the structure. The secular cohort has very low d (-0.10 to 0.10): mobile exit, can choose non-ritual alternatives, may actively reject beneficiary status. The analytical observer has d approaching 0.5 at the universal context: neither beneficiary nor victim, but risk of naturalizing the constraint. These directionality values feed the engine's χ computation (effective extraction), which scales base extractiveness by directionality and scope. High-d agents (diaspora child) experience amplified χ; low-d agents (religious institution) experience dampened or negative χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CASE: This constraint exemplifies delayed mandatrophy — the original founding mandate (transmit collective trauma memory across diaspora when written archives and secular education were unavailable) has been substantially accomplished and rendered partially obsolete by modern infrastructure (documentation, education, memorials). However, the institutional structure persists through: (1) institutional beneficiary resistance (religious authority benefits from ritual perpetuation), (2) identity lock (participants experience participation as constitutive of identity and cannot imagine exit), (3) genuine residual coordination function (ritual does maintain diaspora cohesion, though non-ritual alternatives exist), and (4) cultural narrative that presents the ritual as natural law rather than contingent institutional arrangement. The constraint does not cleanly resolve as mandatrophy because the coordination function is real, even if diminished and potentially replaceable. The scaffold perspective's sunset logic (archival, educational, secular memorial mechanisms maturing) represents one path to mandatrophy resolution, but institutional and identity-lock resistance prevents this path from materializing spontaneously. The theater ratio trajectory (rising from 0.35 to 0.64, then stabilizing at 0.58) is diagnostic of piton-stage mandatrophy: performative content accumulates as functional content diminishes, but the constraint persists due to institutional inertia and theatrical maintenance of meaning. Resolution would require either: (a) voluntary institutional transition to non-ritual memory mechanisms (unlikely given beneficiary structure), (b) exit-enabling reform that reduces identity-lock barriers (difficult and culturally contested), or (c) generational transition as younger participants choose secular memory alternatives (already occurring in some diaspora communities, slower in others).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_necessity_vs_contingency,
    'Is ritual catastrophe-commemoration a natural, unchangeable response to collective trauma transmission across millennia, or a contingent institutional arrangement that could be replaced by archival, educational, or secular memorial mechanisms?',
    'Comparative analysis of diaspora communities with and without ritual commemoration systems; examination of communities that have transitioned from ritual to non-ritual historical transmission (e.g., secular Jewish institutions, historical museums as primary memory venue). Cross-cultural patterns in ritual persistence vs. abandonment.',
    'If natural/necessary: constraint should classify as mountain across all perspectives. If contingent/replaceable: false-summit detector should fire — mountain perspective is naturalizing an extractive institutional arrangement. Classification implications: mountain → rope, or mountain → tangled_rope, depending on beneficiary/victim distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_necessity_vs_contingency, conceptual, 'Whether ritual catastrophe-commemoration is natural law or contingent institution').

omega_variable(
    identity_lock_mechanism_depth,
    'How deeply is individual identity fused with ritual participation in diaspora communities? Is exit truly identity-dissolving, or is the perceived impossibility of exit a product of socialization and can be overcome with sufficient social support?',
    'Longitudinal study of individuals who exit or substantially reduce ritual participation; interviews with lapsed participants about identity stability and reconstruction; comparison of identity coherence in participants vs. lapsed participants 5+ years post-exit.',
    'If deep fusion: identity_locked exit classification is structurally accurate, and the powerless perspective''s snare classification holds. If socialization-dependent: exit classification should downgrade to constrained, and snare classification should shift toward tangled_rope (more agency than initially apparent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_depth, empirical, 'Depth of identity fusion with ritual participation').

omega_variable(
    beneficiary_intentionality,
    'Do religious institutions deliberately maintain ritual catastrophe-commemoration to extract benefits (authority legitimation, resource control, conformity enforcement)? Or is institutional benefit an incidental side effect of solving genuine collective-action problems?',
    'Historical analysis of institutional statements on ritual memory transmission; examination of how institutions have resisted or accommodated alternative memory mechanisms; documentation of institutional resource allocation to ritual maintenance vs. secular education.',
    'If deliberate extraction: constraint is snare or tangled_rope with conscious institutional targeting. If incidental benefit: constraint is more clearly rope with inadvertent institutional advantage. Implications for mandatrophy: if extraction is incidental, reform is possible through redirecting existing structures; if deliberate, structural resistance to mandatrophy resolution is built in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality, empirical, 'Whether institutional benefit is intentional or incidental').

omega_variable(
    historical_nuance_suppression_severity,
    'How much historical nuance and contestable interpretations are genuinely lost or suppressed by standardized ritual commemoration vs. how much could coexist with the ritual?',
    'Content analysis comparing ritual recitation/teaching with academic historiography of the same events; examination of communities that have integrated historical scholarship into ritual contexts; measurement of permitted narrative variation within ritual structures.',
    'If high suppression: victims list (competing narratives, nuance) is structurally real and extraction is higher. If low suppression: constraint is more clearly rope (coordination + manageable cost). Implications for theater ratio: if nuance IS permitted, theater ratio should decrease.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_nuance_suppression_severity, empirical, 'Degree of historical nuance suppression in ritual commemorative structures').

omega_variable(
    diaspora_cohesion_alternative_mechanisms,
    'If ritual catastrophe-commemoration were substantially reduced or eliminated, what alternative mechanisms exist (or could exist) to maintain diaspora cohesion and intergenerational cultural continuity?',
    'Examination of diaspora communities with weak or absent ritual structures; analysis of what mechanisms they employ for cohesion and memory transmission; feasibility studies on implementing secular memorial and educational alternatives.',
    'If robust alternatives exist: scaffold perspective is structurally sound and sunset is plausible. If no alternatives exist or emerge slowly: rope perspective strengthens — the ritual is solving a genuine coordination problem with few substitutes. Implications for mandatrophy: if alternatives exist, resolution is possible; if not, the constraint may be persistent despite extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_cohesion_alternative_mechanisms, empirical, 'Existence and viability of alternative cohesion mechanisms').

omega_variable(
    theater_ratio_generational_drift,
    'Is the measured theater ratio (0.58) stable across generational cohorts, or does theatrical content increase as direct traumatic experience becomes more distant?',
    'Measurement of theatrical vs. functional components of ritual across age cohorts within the same community; comparison of ritual intensity and elaboration over historical periods; interviews with participants about perceived function (processing vs. performance).',
    'If theater increases with generational distance: piton classification strengthens, and the constraint exhibits lifecycle drift toward degradation. If theater is stable: ritual is maintaining functional balance. Measurement implications: theater_ratio trend is diagnostic for piton vs. tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_generational_drift, empirical, 'Drift in theatrical content across generational distance from original catastrophe').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel_flat_control, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cath_mem_tr_t0, catastrophe_memory_kernel_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cath_mem_tr_t250, catastrophe_memory_kernel_flat_control, theater_ratio, 250, 0.42).
narrative_ontology:measurement(cath_mem_tr_t500, catastrophe_memory_kernel_flat_control, theater_ratio, 500, 0.5).
narrative_ontology:measurement(cath_mem_tr_t1000, catastrophe_memory_kernel_flat_control, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(cath_mem_tr_t1500, catastrophe_memory_kernel_flat_control, theater_ratio, 1500, 0.64).
narrative_ontology:measurement(cath_mem_tr_t2000, catastrophe_memory_kernel_flat_control, theater_ratio, 2000, 0.58).

% Extraction over time
narrative_ontology:measurement(cath_mem_be_t0, catastrophe_memory_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cath_mem_be_t250, catastrophe_memory_kernel_flat_control, base_extractiveness, 250, 0.3).
narrative_ontology:measurement(cath_mem_be_t500, catastrophe_memory_kernel_flat_control, base_extractiveness, 500, 0.32).
narrative_ontology:measurement(cath_mem_be_t1000, catastrophe_memory_kernel_flat_control, base_extractiveness, 1000, 0.35).
narrative_ontology:measurement(cath_mem_be_t1500, catastrophe_memory_kernel_flat_control, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(cath_mem_be_t2000, catastrophe_memory_kernel_flat_control, base_extractiveness, 2000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cath_mem_su_t0, catastrophe_memory_kernel_flat_control, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cath_mem_su_t500, catastrophe_memory_kernel_flat_control, suppression_requirement, 500, 0.46).
narrative_ontology:measurement(cath_mem_su_t1000, catastrophe_memory_kernel_flat_control, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(cath_mem_su_t1500, catastrophe_memory_kernel_flat_control, suppression_requirement, 1500, 0.52).
narrative_ontology:measurement(cath_mem_su_t2000, catastrophe_memory_kernel_flat_control, suppression_requirement, 2000, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel_flat_control, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, diaspora_identity_boundary_maintenance).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, intergenerational_authority_transmission).
narrative_ontology:affects_constraint(catastrophe_memory_kernel_flat_control, narrative_canonicalization_suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
