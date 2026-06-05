% ============================================================================
% CONSTRAINT STORY: swap_henrician_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swap_henrician_substitution, []).

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
 *   constraint_id: swap_henrician_substitution
 *   human_readable: Henrician Substitution (Swap Pattern in Protestant Reformation)
 *   domain: historical_epistemology/religious_commitment_systems
 *
 * SUMMARY:
 *   The Henrician Substitution in the English Reformation (c. 1534)
 *   represents a critical instantiation of the 'swap' pattern in commitment
 *   system dynamics: the displacement of papal authority with crown authority
 *   over religious doctrine and institutional structure. This constraint
 *   story analyzes whether this event constitutes a genuine transformation of
 *   religious authority (a new commitment system with its own kernel), or a
 *   mere substitution where the underlying extraction mechanisms and
 *   suppression structures remain constant while only their source changes.
 *   The Library Verdict indicates 'NON-BREAK' status — defenders absorb the
 *   reformation as a coherent unified event despite the presence of
 *   contradictory kernel commitments. The analytical puzzle is whether this
 *   absorption reflects genuine logical synthesis, or narrative maintenance
 *   of an unstable composite that accumulates tensions across generations.
 *   The Henrician substitution is particularly revealing because the crown's
 *   authority grounding is explicitly temporal/institutional (power to
 *   govern) rather than theological (scriptural expertise or apostolic
 *   succession), creating an immediate structural mismatch between the
 *   claimed reading (restoration of scriptural Christianity, return to
 *   primitive church) and the actual kernel transformation (replacement of
 *   ecclesiastical with secular authority grounding). The constraint exhibits
 *   high extractiveness (0.58) due to suppression mechanisms (enforced
 *   conformity, coercion against recusancy, elimination of interpretive
 *   autonomy) that perfectly mirror pre-reformation mechanisms, merely
 *   redirected toward a new source. Theater ratio rises from 0.35
 *   (pre-reformation papal authority, claims coherence despite
 *   contradictions) to 0.72 (reformed institutions performing scriptural
 *   fidelity while operating as top-down hierarchies), indicating increasing
 *   theatricality as the original coordination function (settling doctrinal
 *   chaos) atrophies and only the legitimating narrative persists. The
 *   constraint family (if decomposed) would include: (1) the kernel-level
 *   transformation (papal → crown authority grounding), (2) the doctrinal
 *   coherence problem (how reformed theology reconciles sola scriptura with
 *   clerical authority), (3) the lay autonomy suppression (transition from
 *   papal monopoly to crown monopoly on interpretation). This story focuses
 *   on the substitution mechanism itself.
 *
 * KEY AGENTS:
 *   - Henry VIII and Crown Authority: Institutional beneficiary (institutional/arbitrage) — consolidates religious authority under secular control, eliminates papal revenue claims, establishes supremacy over doctrinal and disciplinary matters.
 *   - Lay Believers (Reformed Identity): Primary victim and identity-locked (powerless/identity_locked) — structurally mobile but identity-fused with reformed identity; exit would require ego death; experience maximum extraction despite liberation narrative.
 *   - Parish Clergy: Secondary victim and constrained moderate (moderate/constrained) — experience both coordination benefit (clearer hierarchy, unified governance) and extraction (loss of institutional autonomy, subjection to secular authority).
 *   - Papal Institution (Continental Scale): Institutional victim at distance (institutional/constrained) — loses authority and revenue in affected territories; forced coordination response (Counter-Reformation) to address doctrinal incoherence exposed by reformation.
 *   - Established Reformed Institutions: Institutional inertia by civilizational scale (institutional/arbitrage) — perform reformed identity while replicating papal structure; theater ratio rises as functional justification atrophies.
 *   - Enlightenment Intellectual Coalition: Organized agents building exit paths (organized/mobile) — develop alternative authority grounding (secular reason, consent, constitution) providing genuine scaffold with sunset clause on religious coercion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swap_henrician_substitution, 0.58).
domain_priors:suppression_score(swap_henrician_substitution, 0.65).
domain_priors:theater_ratio(swap_henrician_substitution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swap_henrician_substitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(swap_henrician_substitution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(swap_henrician_substitution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swap_henrician_substitution, tangled_rope).
narrative_ontology:human_readable(swap_henrician_substitution, "Henrician Substitution (Swap Pattern in Protestant Reformation)").
narrative_ontology:topic_domain(swap_henrician_substitution, "historical_epistemology/religious_commitment_systems").

domain_priors:requires_active_enforcement(swap_henrician_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swap_henrician_substitution, crown_authority).
narrative_ontology:constraint_beneficiary(swap_henrician_substitution, established_clergy).
narrative_ontology:constraint_beneficiary(swap_henrician_substitution, reform_defenders).
narrative_ontology:constraint_victim(swap_henrician_substitution, papal_institutional_coherence).
narrative_ontology:constraint_victim(swap_henrician_substitution, doctrinal_consistency_claim).
narrative_ontology:constraint_victim(swap_henrician_substitution, lay_interpretive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY BELIEVER (SNARE) — Structurally mobile (could reject reform, emigrate, conform outwardly) but identity-fused with reformation identity. The reform's framing ('returning to pure scripture,' 'restoring primitive Christianity') has constituted the believer's self-understanding. Exit would require not just changing affiliation but abandoning the identity that the constraint established. Suppression is high: coercive authority structures (crown enforcement, reformed church discipline) replace papal authority without reducing coercion. Theater is high: the Reformation's legitimacy narrative (recovery of authentic Christianity) masks the substitution of one authority structure for another. Extraction reaches maximum because the believer experiences no exit option that preserves selfhood.
constraint_indexing:constraint_classification(swap_henrician_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: PARISH CLERGY (TANGLED ROPE) — Face genuine coordination problem: consolidating authority under the crown reduces doctrinal chaos and clarifies ecclesiastical hierarchy. But also face extraction: authority now flows through royal courts rather than Rome, reducing clerical autonomy and increasing subjection to secular power. Constrained exit: could resist (career destruction, execution risk) or conform (career stability, but under crown rather than papal authority). The coordination function (unified church governance) is genuine; the extraction (loss of institutional independence) is also genuine. This is the agent experiencing the hybrid most acutely.
constraint_indexing:constraint_classification(swap_henrician_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CROWN AUTHORITY (ROPE) — Experiences the reformation as pure coordination: consolidating religious authority under secular control solves the governance problem of papal interference in temporal affairs. Arbitrage exit available (could have chosen different religious settlement, but the reformation is the preferred option because it maximizes authority consolidation). Net beneficiary — the constraint subordinates ecclesiastical authority to crown will. Theater is moderate from this perspective: the crown can justify the reformation through coordination language (unified church, simplified doctrine) while simultaneously using it to extract authority and revenue from former papal assets.
constraint_indexing:constraint_classification(swap_henrician_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PAPAL INSTITUTION (TANGLED ROPE) — At the continental scale, Rome experiences the reformation as extraction: loss of territorial authority, revenue streams, and institutional reach. But there is also a coordination element that the papal institution must absorb: the reformation exposes genuine doctrinal incoherence in late medieval Catholicism (indulgence trade, clerical corruption), and Rome's Counter-Reformation response creates a functioning coordination mechanism (clarified doctrine, disciplined clergy via Council of Trent). Constrained exit for the institution: could have suppressed the reformation earlier (pre-Luther), but by the time of Henrician substitution, the institutional incoherence has already metastasized. Rome's coordination response is forced, not chosen — this is constrained, not mobile or arbitrage.
constraint_indexing:constraint_classification(swap_henrician_substitution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REFORMED INSTITUTIONAL LEGACY (PITON) — Three centuries later, the reformed settlement has become institutional inertia: Protestant established churches replicate the papal structure they rejected (hierarchical clergy, doctrinal authority, enforcement through coercion) while maintaining the legitimating narrative of reformation freedom. Theater is high: the institutions perform 'scriptural fidelity' and 'congregational autonomy' while operating as top-down authority structures identical in function to what they replaced. The reformed settlement persists not because its claimed coordination function works but because alternative institutional arrangements haven't fully emerged. This is the engine of degradation: the settlement's original function (consolidating authority, settling doctrinal chaos) was real; by the civilizational perspective, the function has atrophied and only the narrative persists.
constraint_indexing:constraint_classification(swap_henrician_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ENLIGHTENMENT INTELLECTUAL COALITION (SCAFFOLD) — By the 18th century, organized agents (Enlightenment thinkers, liberal political movements) begin building alternative verification pathways: secular authority grounded in reason and consent rather than any theological kernel. This perspective sees the reformation's swap as a temporary settlement with a sunset: as secular institutions mature (constitutional government, civil law, public reason), religious authority becomes optional rather than coercive. Mobile exit emerges: agents can exit religious authority structures without losing political participation or citizenship. The scaffold is genuine — but only from the civilizational/generational perspective where intellectual movements can accumulate. From the biographical perspective of lay believers still identity-locked within reformed identity, the scaffold is invisible.
constraint_indexing:constraint_classification(swap_henrician_substitution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, the reformation might appear as an immutable structural transition: every polity must settle the relationship between religious and secular authority, and the swap pattern (replace one authority source with another, narrate it as liberation) is a recurring historical structure. This naturalizes the swap as inevitable. However, the engine will detect this as a false summit: the swap is not logically necessary but historically contingent. Alternative settlements existed (Spain/Portugal maintained crown-papal fusion; Netherlands developed sectarian pluralism; later settlements decoupled religious and secular authority entirely). The mountain classification masks the contingency and beneficiary structure.
constraint_indexing:constraint_classification(swap_henrician_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swap_henrician_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swap_henrician_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swap_henrician_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(swap_henrician_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(swap_henrician_substitution, TR),
    TR >= 0.70.

:- end_tests(swap_henrician_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Measured as the asymmetric concentration of interpretive and institutional authority following the substitution. Pre-reformation extractiveness was high (0.52) due to papal monopoly on doctrine and clerical control over access to scripture. Post-Henrician extractiveness increases slightly (0.58) because the crown authority is more coercive (secular enforcement capacity exceeds ecclesiastical capacity), suppresses interpretive diversity more effectively (Protestant consensus harder to disagree with than medieval Catholicism's internal contradictions), and offers no alternative institution (papal authority is eliminated, not pluralized). The modest increase (0.06 points) reflects that the coordination function is real — the settlement does reduce doctrinal chaos — but the extraction mechanism is also real: lay believers lose access to interpretive authority even as they are told they have gained it (sola scriptura doctrine vs clerical monopoly on interpretation). By the civilizational perspective (t=200), extractiveness begins declining (0.58 → 0.45 → 0.32) as secular authority emerges, offering genuine exit alternatives. Suppression (0.65): Measured as the coercive intensity and elimination of alternatives. Henrician suppression includes: recusancy penalties, execution of religious dissenters, elimination of monastic institutions as alternative authority sources, enforcement of conformity through parish discipline, prohibition of interpretive diversity. The suppression is high because alternatives are structurally eliminated (no competing church, no religious dissent permitted, no emigration options for most believers). Theater ratio (0.68): Pre-reformation theater is moderate (0.35) because papal authority's incoherence is visible and contested. Henrician settlement theater increases (0.58 at t=50) because the legitimating narrative (return to scripture, liberation from papal corruption) obscures the substitution of mechanisms. Theater peaks (0.72 at t=200) when reformed institutions have fully replicated papal structures while maintaining reformation narrative. The plateau then declines as secular alternatives become visible (t=250+), making the reformation narrative less credible. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (settling doctrinal chaos, clarifying authority lines) and asymmetric extraction (concentration of authority, suppression of lay interpretation, coercion). The base properties reflect the equilibrium after the swap: the coordination function has stabilized (lay believers accept reformed authority as legitimate), but the extraction mechanism remains (interpretive monopoly continues under a different source).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the lay believer's Snare and the crown authority's Rope is maximal — they are perceiving the same constraint as opposite types. This gap reveals that the constraint's extractiveness is not inherent to the structure but relative to the agent's power and exit options. The gap between the piton (reformed institutions at civilizational scale) and the rope (crown authority at immediate scale) reveals that the same institutional structure has opposite functional profiles across time horizons: at the immediate horizon it coordinates governance; at the civilizational horizon it persists through theater. The gap between the scaffold (enlightenment coalition) and the snare (lay believer) reveals that organized agents with intellectual resources can perceive exit paths that powerless agents cannot — the constraint appears immutable from below, temporary from above. These gaps collectively demonstrate why single-perspective classification fails for this constraint and why the six-type ecosystem is necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each perspective's power level, exit options, and relationship to the authority flow. The lay believer's d is extremely high (~0.95) because they are powerless, identity-locked (structurally mobile but cognitively trapped), and the extraction flows away from them — they bear the suppression and lose interpretive autonomy. The parish clergy's d is moderate (~0.55) because they are moderate power, constrained exit (career destruction if they resist, but conformity is possible), and experience both benefit (clearer hierarchy, career stability) and cost (loss of institutional autonomy). The crown's d is extremely low (~0.05) because they are institutional, arbitrage exit (the reformation is their chosen option), and beneficiaries of the extraction. The papal institution's d is high (~0.78) at continental scale because they are institutional but constrained (forced response, not choice), and targets of extraction (territorial loss, revenue loss). The reformed institutions' d remains around the original substitution level but their f(d) declines as the theater ratio rises (the sigmoid f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50))) dampens as d becomes static but context changes). The enlightenment coalition's d is high (~0.89) in their capacity as organizers of an alternative but they have mobile exit options (~0.40 derived d), producing moderate experienced extraction. The analytical observer's canonical d is high (~0.73) but the mountain classification attempts to override through naturalization — the engine's false summit detector should catch this.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that it is NOT asking 'which type is correct?' but 'which commitment system dynamic is the reformation instantiating?' The answer is SWAP: replacement of one authority grounding with another while maintaining the same suppression and coercion mechanisms. This is distinct from CLIMB (transformation of authority into a higher-order system), EMERGENCE (creation of a new kernel not previously present), or DROP (dissolution of an authority system). The mandatrophy is resolved by noting that the reformation appears to be a CLIMB (liberation from papal corruption, return to scripture, elevation of lay participation) in the beneficiary's and reform-defender's narrative, but structurally it is a SWAP (papal authority → crown authority, no reduction in suppression, no genuine increase in lay autonomy). The lay believer's identity-lock in the reformed identity makes them unable to perceive the swap — they experience the liberation narrative as reality. The analytical observer, if they naturalize the reformation as inevitable, also miss the swap — they treat it as necessary rather than contingent. The mandatrophy is fully resolved when the analytical position includes the explicit recognition that the substitution is contingent and that the beneficiary/victim structure reveals the swap mechanism. No reclassification is needed; the classification schema captures the structure correctly. The constraint is Snare for lay believers precisely because they cannot perceive the swap from within their identity frame. The constraint is Tangled Rope for clergy precisely because they experience both the coordination and extraction simultaneously. The constraint is Piton for civilizational observers precisely because the functional justification has atrophied and narrative persists. All six types are correct from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    swap_vs_transformation_ambiguity,
    'Is the Henrician substitution a genuine transformation of authority sources (swap: papal → crown), or a surface substitution masking continuity of authority structure and suppression mechanisms?',
    'Comparative institutional analysis: measure continuity of (a) enforcement mechanisms, (b) revenue flows, (c) doctrinal authority, (d) coercive capacity across the substitution point. If all four show continuity in mechanism but change in source, it is a pure swap. If the mechanisms themselves were structurally reformed, it is a genuine transformation.',
    'If pure swap: the reformation is a Snare for lay believers and a Piton for reformed institutions (function atrophied, structure persists). If genuine transformation: the reformation might be a Rope or legitimate Scaffold with real coordination gains. Classification swings on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swap_vs_transformation_ambiguity, empirical, 'Whether the substitution is swap-only or genuine structural transformation').

omega_variable(
    composite_kernel_vs_single_reading,
    'Does the Reformation instantiate one kernel (religious authority grounding) with competing readings (papal vs reformed readings of scripture), or multiple distinct kernels (authority grounding, doctrinal coherence, lay interpretive access) that defenders absorb as unified?',
    'Logical decomposition: identify the irreducible commitments each reading requires. If all readings share the same kernel commitment (e.g., ''Christianity requires hierarchical authority'') but disagree on the correct hierarchy, it is one kernel with competing readings. If readings require incompatible kernel commitments (e.g., reformed reading requires ''scripture alone'' while papal reading requires ''tradition + magisterium''), these are multiple kernels.',
    'If one kernel: the reformation is a reading contest within a unified commitment system (CS-pattern applies). Classification may shift to mountain (kernel immutable, readings contested) or tangled_rope (competing readings extract from other positions). If multiple kernels: the reformation is an overdetermined event where defenders synthesize incompatible commitments to maintain coherence. Amplifies the false-summit risk — what appears as a natural historical progression is actually absorbing fundamental inconsistencies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(composite_kernel_vs_single_reading, conceptual, 'Whether reformation is one kernel with multiple readings or composite of multiple kernels').

omega_variable(
    identity_lock_mechanism_interpersonal_scale,
    'What specific identity-fusion mechanisms bind lay believers to reformed identity such that exit appears unthinkable even when external barriers are removed (or could be removed via emigration)?',
    'Historical narrative analysis: identify the constitutive elements of reformed identity as presented in contemporary sermons, catechisms, and theological writings. Cross-reference with modern identity-lock case studies (cult deprogramming, radical group exit, trauma-bonded relationships) to identify analogous binding mechanisms. Test whether believers who relocated to non-reformed regions maintained reformed identity despite freedom to exit.',
    'If identity-lock mechanisms are strong: lay believers genuinely cannot exit without ego death, classifying the constraint as Snare for them (high d from identity-lock + powerless/trapped). If mechanisms are weak or artificially imposed: the suppression is structural rather than internalized, classifying the constraint differently. This determines whether the reformation''s extractiveness is inherent or contingent on the identity-lock narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal_scale, empirical, 'Identity-fusion mechanisms binding reformed believers to reformation identity').

omega_variable(
    library_verdict_nonbreak_paradox,
    'The Library Verdict indicates ''NON-BREAK'' status: defenders absorb the composite (multiple incompatible kernels) as unified without the institutional fabric breaking. But how do defenders maintain coherence when the kernels contradict? What is the absorption mechanism?',
    'Trace institutional reconciliation moves: identify how reformed institutions and theorists resolved each kernel contradiction (scripture-alone vs authority hierarchy, lay access vs clerical monopoly, temporal power vs spiritual purity). Categorize resolutions as: (1) logical synthesis (genuine reconciliation), (2) compartmentalization (keeping incompatible commitments in separate institutional contexts), (3) narrative absorption (reframing contradictions as appearances of deeper unity).',
    'If resolutions are logical syntheses: the reformers created a coherent new commitment system (the contradiction was apparent, not real). If compartmentalized: the reformation created an inherently unstable structure generating cascading schisms. If narrative absorption: the reformation is a false unity masking fundamental tensions — the constraint persists through enforced coherence rather than achieved consistency. This directly determines whether the settlement is sustainable (Scaffold with sunset) or degrading (Piton with inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(library_verdict_nonbreak_paradox, empirical, 'Mechanism by which defenders absorb contradictory kernels without institutional break').

omega_variable(
    henrician_vs_calvinist_divergence,
    'Do the Henrician (crown-episcopal) and Calvinist (congregational) reformations represent different swaps of the same kernels, or fundamentally different transformations? If the same kernels, why do they produce incompatible institutional outcomes?',
    'Map the kernel commitments of each reformation: Henrician (authority consolidation, doctrinal simplification, secular-religious fusion), Calvinist (scriptural primacy, doctrinal rigor, congregational autonomy). Identify which kernels each prioritizes and which it marginalizes. If they prioritize the same set in different orders, it is the same kernels with different readings. If they prioritize incompatible kernel sets, they are distinct substitutions with distinct extractiveness profiles.',
    'If same kernels: this demonstrates that the reformation is not a single logical structure but a contested field where multiple incompatible readings of the same commitments generate competing institutions. The Snare classification (for lay believers) applies differently to Henrician vs Calvinist contexts — identity-lock mechanisms differ. If distinct kernels: the ''Protestant Reformation'' is a historiographical unity masking multiple distinct events. Extractiveness should be recomputed separately for each.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(henrician_vs_calvinist_divergence, empirical, 'Whether Henrician and Calvinist reformations use same kernels with different readings or distinct kernel sets').

omega_variable(
    doctrinal_vs_institutional_extraction,
    'Is the primary extraction mechanism doctrinal (control of interpretation, monopoly on correctness) or institutional (control of authority, monopoly on legitimacy)? Or are they inseparable in this context?',
    'Separate the two axes: (1) doctrinal control: measure the range of acceptable theological positions, enforcement against heterodoxy, penalties for interpretive deviation. (2) Institutional control: measure the concentration of authority, barriers to parallel institutional emergence, revenue flows. If extractiveness correlates primarily with doctrinal range restriction, the mechanism is doctrinal. If it correlates with institutional monopoly, the mechanism is institutional. If both are equally strong, they are inseparable.',
    'If doctrinal: the reformation might be reclassified as identity_coordination type in Boltzmann analysis — the extraction is through membership boundary maintenance and correctness claims. If institutional: it is resource_allocation or enforcement_mechanism type. If inseparable: the constraint genuinely coordinates doctrine (prevents chaos) while extracting institutional power (concentrates authority). This preserves the Tangled Rope classification but clarifies the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_institutional_extraction, empirical, 'Whether extraction is primarily doctrinal or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swap_henrician_substitution, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_reformation, swap_henrician_substitution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_henrician_settlement, swap_henrician_substitution, theater_ratio, 50, 0.58).
narrative_ontology:measurement(theater_established_reformed, swap_henrician_substitution, theater_ratio, 150, 0.68).
narrative_ontology:measurement(theater_piton_phase, swap_henrician_substitution, theater_ratio, 200, 0.72).

% Extraction over time
narrative_ontology:measurement(extractiveness_pre_reformation, swap_henrician_substitution, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(extractiveness_henrician_settlement, swap_henrician_substitution, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(extractiveness_established_reformed, swap_henrician_substitution, base_extractiveness, 150, 0.61).
narrative_ontology:measurement(extractiveness_piton_decline, swap_henrician_substitution, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(extractiveness_enlightenment_erosion, swap_henrician_substitution, base_extractiveness, 250, 0.45).
narrative_ontology:measurement(extractiveness_modern_pluralism, swap_henrician_substitution, base_extractiveness, 300, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swap_henrician_substitution, enforcement_mechanism).
narrative_ontology:affects_constraint(swap_henrician_substitution, sola_scriptura_interpretation_monopoly).
narrative_ontology:affects_constraint(swap_henrician_substitution, lay_interpretive_autonomy_suppression).
narrative_ontology:affects_constraint(swap_henrician_substitution, papal_authority_territorial_loss).

% DUAL FORMULATION NOTE:
% The Henrician substitution is part of a reformation constraint family. Upstream: the papal institutional incoherence and late-medieval doctrinal chaos that created demand for reform. Downstream: (1) the interpretation monopoly problem (sola scriptura doctrine vs clerical control), (2) the lay autonomy suppression (promise of lay access to scripture vs actual restriction of interpretive authority), (3) the papal institutional response (Counter-Reformation, retrenchment of authority). The substitution story models the swap mechanism itself; the decomposed stories model the specific contradiction kernels it absorbs without breaking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(swap_henrician_substitution, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
