% ============================================================================
% CONSTRAINT STORY: brahmanical_commentary_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brahmanical_commentary_authority, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brahmanical_commentary_authority
 *   human_readable: Brahmanical Commentary Authority and Interpretive Accretion
 *   domain: religion/hindu_authority/textual_interpretation
 *
 * SUMMARY:
 *   The Brahmanical commentary (bhasya) tradition represents a sophisticated
 *   mechanism for absorbing operational drift while preserving textual
 *   authority. The Vedic kernel is treated as immutable and complete;
 *   centuries of institutional, social, and theological change are processed
 *   through commentary as explication of meanings always-already-implicit in
 *   the kernel. Major commentators (Shankara, Ramanuja, Madhva) produced
 *   radically incompatible interpretations of Upanishadic texts, yet each
 *   claimed to be revealing the kernel's true meaning, not constructing new
 *   meaning. This constraint exhibits the full range of Deferential Realism
 *   classifications from different structural positions: for non-Brahmins
 *   dependent on Brahmin mediation of religious meaning, it functions as a
 *   Snare (interpretive authority is monopolized and extraction flows toward
 *   the priesthood); for heterodox sects challenging Vedic authority, it
 *   functions as a Tangled Rope (they coordinate their own religious practice
 *   while bearing costs of delegitimization within the larger framework); for
 *   Brahmin commentators themselves, it functions as a Rope (legitimate
 *   epistemic coordination of theological interpretation); for the
 *   institutional ritual establishment, as a Piton (theatrical maintenance of
 *   cosmological efficacy claims); for rival Brahmin lineages, as a Tangled
 *   Rope (genuine theological coordination paired with zero-sum competition
 *   for patronage); and for the civilizational analytical observer, it risks
 *   appearing as a Mountain (natural feature of religious textual authority)
 *   — though structural analysis reveals this as a false summit (the
 *   configuration benefits identifiable agents and rests on contingent
 *   institutional arrangements, not natural law). The constraint demonstrates
 *   how interpretive accretion can function as an indefinite maintenance
 *   system for authority: new operational realities are incorporated as
 *   implicit meanings rather than requiring kernel revision, which would
 *   expose the kernel as non-self-sufficient.
 *
 * KEY AGENTS:
 *   - Brahmin Priesthood: Primary beneficiary (institutional/arbitrage) — monopolizes interpretive access, extracts deference and material support (landholdings, tax exemptions, gift income), enjoys social status from authority position
 *   - Non-Brahmin Religious Practitioners: Primary victims (powerless/trapped) — dependent on Brahmin mediation for legitimate religious knowledge; no alternative pathway to textual access; trapped by ritual obligation to accept Brahmin interpretation
 *   - Heterodox Sects (early Buddhism, Jainism, non-Brahmin theisms): Secondary victims (moderate/constrained) — develop alternative interpretations but face delegitimization; constrained by need to demonstrate Vedic compatibility to gain social acceptance
 *   - Rival Brahmin Lineages (Advaita, Visistadvaita, Dvaita): Inter-institutional competitors (institutional/constrained) — produce incompatible interpretations while competing for patronage and institutional control; genuine theological coordination paired with zero-sum political extraction
 *   - Vedic Ritual Establishment: Institutional actor (institutional/arbitrage) — maintains ritual performance structures (yajnas, soma ceremonies) whose cosmological efficacy is questioned; persists through theatrical maintenance and inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing kernel-commentary structure as immutable feature of religious authority rather than recognizing it as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brahmanical_commentary_authority, 0.52).
domain_priors:suppression_score(brahmanical_commentary_authority, 0.58).
domain_priors:theater_ratio(brahmanical_commentary_authority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brahmanical_commentary_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(brahmanical_commentary_authority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(brahmanical_commentary_authority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brahmanical_commentary_authority, tangled_rope).
narrative_ontology:human_readable(brahmanical_commentary_authority, "Brahmanical Commentary Authority and Interpretive Accretion").
narrative_ontology:topic_domain(brahmanical_commentary_authority, "religion/hindu_authority/textual_interpretation").

domain_priors:requires_active_enforcement(brahmanical_commentary_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brahmanical_commentary_authority, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(brahmanical_commentary_authority, vedic_authority_maintenance).
narrative_ontology:constraint_victim(brahmanical_commentary_authority, non_brahmin_religious_practitioners).
narrative_ontology:constraint_victim(brahmanical_commentary_authority, alternative_interpretive_lineages).
narrative_ontology:constraint_victim(brahmanical_commentary_authority, textual_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-BRAHMIN PRACTITIONER (SNARE) — Cannot access interpretive authority directly; dependent on Brahmin mediation of textual meaning. Trapped by ritual obligation to accept Brahmin-transmitted interpretation; no alternative pathway to legitimate religious knowledge. Maximum extraction — the commentary tradition coordinates religious practice while extracting deference and material support (gifts, landholdings, tax exemptions) in exchange for interpretive access.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: HETERODOX SECT / ALTERNATIVE LINEAGE (TANGLED ROPE) — Can develop own interpretations (mobile exit in principle) but faces severe social and institutional penalties for claiming authority. Benefits from Vedic kernel legitimacy even while challenging Brahmin interpretation monopoly. Constrained by need to demonstrate Vedic compatibility to gain acceptance — must work within kernel-commentary framework even when that framework extracts from them. Mixed extraction and coordination: they coordinate religious practice within their sect while bearing cost of constant delegitimization from Brahmin authorities.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMIN COMMENTATOR (ROPE) — Experiences the commentary tradition as legitimate epistemic coordination: commentators solve the genuine problem of making implicit textual meanings operational across generations. Each commentator (Shankara, Ramanuja, Madhva) produces incompatible interpretations but within the framework of explicating rather than revising the kernel. The commentator sees this as intellectual freedom and scholarly rigor, not as extractive gatekeeping. Benefits from commentary authority through patronage, landholdings, and social deference. Arbitrage exit available: a brilliant commentator can establish new interpretation lineage.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: VEDIC RITUAL ESTABLISHMENT (PITON) — The institutional apparatus of Vedic ritual (yajnas, soma ceremonies, Vedic school curricula) persists largely through theatrical maintenance: the actual cosmological efficacy attributed to rituals is questioned even within Brahmin intellectual circles, yet the ritual structure persists as a marker of Brahmin authority and legitimacy. Theater ratio high: the performance of Vedic ritual coordination exceeds its functional necessity. The establishment maintains itself through inertia — the commentary tradition keeps the kernel alive even as the ritual mechanisms lose operational force.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURALIZED AUTHORITY VIEW (MOUNTAIN) — From a civilizational perspective, the kernel-commentary structure appears as a natural feature of religious authority: textual traditions require interpretation, interpretation requires specialists, specialists require authority to mediate meaning. The Vedic kernel is unchanging; commentary is the natural mechanism for making implicit meanings explicit. This perspective risks naturalizing what is actually a contingent institutional arrangement — the claim that 'interpretation is natural' masks the extraction of authority monopoly.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: RIVAL BRAHMIN LINEAGE (TANGLED ROPE) — Competing Brahmin schools (Shankara Advaita vs Ramanuja Visistadvaita vs Madhva Dvaita) genuinely coordinate theological interpretation while extracting from each other through zero-sum competition for patronage, institutional control, and interpretive authority. Constrained by inability to revise the kernel or abandon the commentary framework — doing so would lose Vedic legitimacy. Mixed benefit and cost: legitimate intellectual coordination within brahminical circles but asymmetric extraction at the inter-lineage level based on political and patronage power.
constraint_indexing:constraint_classification(brahmanical_commentary_authority, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brahmanical_commentary_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brahmanical_commentary_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brahmanical_commentary_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brahmanical_commentary_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brahmanical_commentary_authority, TR),
    TR >= 0.70.

:- end_tests(brahmanical_commentary_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Brahmin priesthood extracts significant material and social value (landholdings, tax exemptions, ritual fees, patronage income, social deference) from their monopoly on interpretive authority. However, the extraction is not total because legitimate coordination functions exist — the commentary tradition does enable theological development and religious practice across generations. The measurement reflects the period 0-10 (roughly first-millennium CE), where Brahmin institutional dominance is growing but not yet absolute. From the powerless non-Brahmin perspective (trapped exit), extractiveness appears maximal (Snare at 0.95+); from the Brahmin perspective (arbitrage exit), it appears minimal (Rope with negative chi). The base_extractiveness of 0.52 is the scalar used in chi calculations for mixed-exit perspectives (moderate agents with constrained options). Suppression (0.58): Moderate-high. Multiple barriers prevent non-Brahmins from claiming interpretive authority: ritualistic gatekeeping (only Brahmin-born individuals can perform certain rituals and therefore cannot achieve the experiential authority to interpret them), educational monopoly (Vedic schooling restricted to Brahmins), social sanctions against heterodox interpretation (relegation to lower ritual status or exclusion from religious community), and the kernel-commentary framework itself (claiming to innovate rather than interpret threatens one's legitimacy). These are structural suppressions enforced through social and institutional mechanisms. However, suppression is not absolute — heterodox sects do emerge and develop alternative traditions, particularly where economic and political power shifts away from Brahmin control. Theater ratio (0.68): Moderate-high. The Vedic ritual establishment shows significant theatrical content: the cosmological efficacy attributed to Vedic rituals (the idea that yajnas maintain cosmic order) is questioned even within sophisticated Brahmin intellectual circles, yet ritual performance persists as a marker of Brahmin authority. The ritual apparatus (exact meter, correct pronunciation, precise gesture) is given cosmic significance despite functional redundancy in post-vedic religious practice. Commentary tradition itself exhibits theater: the claim that commentaries are merely explicating implicit meaning (rather than constructing meaning) becomes increasingly theatrical as incompatible commentaries multiply — the shared illusion that all commentaries serve the same kernel obscures that each is reconstructing the kernel according to its own philosophical commitments. The measurement trajectory (0.52 → 0.68 over the interval) reflects increasing sophistication in maintaining the illusion of kernel-preservation while actually conducting systematic reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The non-Brahmin practitioner (powerless/trapped) experiences a Snare: interpretive authority is monopolized, no alternative exists, and material extraction flows toward the priesthood. The heterodox sect (moderate/constrained) experiences a Tangled Rope: they can develop their own interpretation (mobile exit in principle) but face severe delegitimization penalties and must work within kernel-commentary framework to gain legitimacy (constrained exit in practice). The Brahmin commentator (institutional/arbitrage) experiences a Rope: they are solving the genuine problem of making implicit textual meanings operational; they enjoy intellectual freedom to produce incompatible interpretations while remaining within the single framework; and they benefit from patronage and authority. The rival Brahmin lineage (institutional/constrained) experiences a Tangled Rope: genuine theological coordination coexists with zero-sum competition for patronage. The ritual establishment (institutional/arbitrage) experiences a Piton: ritual performance persists through theatrical maintenance and institutional inertia. The analytical observer (analytical/analytical) risks experiencing a Mountain: the kernel-commentary structure appears as a natural feature of how religious traditions work, obscuring that it is a contingent institutional arrangement that benefits Brahmins. The perspectival gap is not a measurement error but a structural feature: the same configuration looks like coordination to insiders (Rope) and extraction to outsiders (Snare) precisely because it achieves extraction through coordination mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is derived from the agent's structural position relative to the interpretation-authority flow. Beneficiaries (Brahmin priesthood) have arbitrage exit options — they can relocate, establish new interpretation schools, or adapt to new patronage arrangements. From the beneficiary + arbitrage position, the engine derives low d (approximately 0.15), which feeds negative effective extraction (chi). The original research group in the verification bottleneck example has similar structure: beneficiary from first-mover advantage, arbitrage exit, low experienced extraction (Rope perspective). Victims with trapped exit (non-Brahmin practitioners) face maximum d (approximately 0.95), which feeds high f(d) and high chi — they experience maximum extraction. Constrained victims (heterodox sects) face intermediate d (approximately 0.70), producing moderate-high chi (Tangled Rope). The rival Brahmin lineage faces a different directionality problem: both are beneficiaries (they both extract authority from the kernel-commentary framework), but they face competition rather than complementarity. The engine's standard derivation treats both as beneficiaries with arbitrage exits, but the directional asymmetry between them (which lineage gains patronage dominance) is determined by external political and economic factors, not by the constraint structure itself. This is a case where directionality_overrides might be appropriate — if one Brahmin lineage achieves dominance through political force rather than intellectual merit, its directionality should be adjusted upward from the standard beneficiary + arbitrage value to reflect its predatory relationship to rival lineages.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the tangled_rope classification is the analytical truth while rope and snare are perspectival truths. The base properties show genuine coordination (commentary tradition does enable theological development and religious practice across generations) paired with genuine asymmetric extraction (Brahmin priesthood monopolizes interpretive authority and extracts material and social benefit). The constraint requires active enforcement (alternative interpretation must be delegitimized; ritual gatekeeping must be maintained; educational monopoly must be protected). The beneficiaries (Brahmin priesthood) and victims (non-Brahmin practitioners, heterodox sects) are identifiable and structural. The configuration fits tangled_rope gates: 0.40 ≤ χ ≤ 0.90 (effective extraction depends on the observer's power level and exit options), base extraction ε ≥ 0.30 (0.52 in this case), suppression ≥ 0.40 (0.58 in this case). The rope classification (from Brahmin commentator perspective) captures the genuine coordination function. The snare classification (from non-Brahmin practitioner perspective) captures the genuine extraction and suppression. The analytical perspective risks classifying as mountain (naturalizing the kernel-commentary structure), which the false summit detection system catches via the beneficiary declaration. The mandatrophy is resolved: this is authentically a tangled_rope constraint, not mislabeled pure extraction or mislabeled pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_versus_constructed,
    'Are the meanings that commentary tradition claims to explicate genuinely implicit in the Vedic kernel, or are they constructed through the commentary process itself?',
    'Historical-textual analysis: do commentaries consistently uncover the same meanings, or do they diverge fundamentally? Do the meanings commentaries attribute to kernel passages appear in pre-commentary textual traditions? Do alternative interpretive frameworks applied to the same kernel texts yield comparable results?',
    'If genuinely implicit: commentary tradition is coordination mechanism (Rope from Brahmin perspective, Mountain from analytical). If constructed: commentary tradition is extractive interpretive monopoly (Snare from non-Brahmin perspective, Tangled Rope from analytical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_versus_constructed, conceptual, 'Whether commentary tradition reveals implicit or constructs meaning').

omega_variable(
    kernel_stability_boundary,
    'What constitutes a violation of the kernel-commentary boundary? At what point does a commentary become kernel-revision rather than interpretation?',
    'Corpus analysis: identify cases where a commentary was rejected as transgressing the kernel-commentary boundary and those where radical reinterpretation was accepted as legitimate commentary. Map the social/political factors that determine boundary enforcement.',
    'If boundary is rigidly enforced: suppression is structural (constraint is tight Snare). If boundary is flexible and permeable: suppression is lower (constraint becomes Rope with commentary-as-innovation). If boundary is determined by patronage power rather than textual criteria: extraction mechanism is transparent (constraint clearly Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_stability_boundary, empirical, 'Definition of the kernel-commentary interpretive boundary').

omega_variable(
    material_extraction_quantification,
    'What proportion of Brahmin wealth and landholding stemmed from their interpretive/ritual monopoly versus their role as landowners and social administrators?',
    'Archival analysis of land records, gift inscriptions, and patronage networks. Comparative analysis with non-Brahmin wealth-holding patterns. Counterfactual: would Brahmin prosperity occur without interpretive authority?',
    'If interpretive monopoly is primary driver of extraction: constraint classification remains Tangled Rope/Snare (extraction is tied to interpretive gatekeeping). If incidental to larger landholding system: constraint is more accurately Rope (commentary tradition enables real coordination, and material benefits are secondary patronage reward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_extraction_quantification, empirical, 'Quantification of material extraction from interpretive authority').

omega_variable(
    heterodox_assimilation_mechanism,
    'When heterodox sects (Buddhism, Jainism, non-Brahmin theisms) explicitly reject Vedic authority, how does the Brahmin establishment respond? Does it attempt integration (absorbing heterodox ideas as ''implicit in kernel''), delegitimization, or coexistence?',
    'Historical analysis of brahminical responses to heterodoxy. Document cases where heterodox ideas were integrated into brahminical interpretation vs. explicitly rejected. Map temporal patterns: does integration increase over time (indicating commentary-as-absorption mechanism) or remain constant?',
    'If integration is primary strategy: the kernel-commentary system is absorptive (extractive mechanism works by subordinating alternatives). If delegitimization is primary: suppression is more nakedly structural (Snare becomes clearer). If coexistence: constraint may be weaker than Tangled Rope classification suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heterodox_assimilation_mechanism, empirical, 'Brahmin response mechanism to heterodox authority challenges').

omega_variable(
    false_summit_natural_law,
    'Is the kernel-commentary structure a natural feature of religious textual authority (mountain) or a contingent institutional arrangement that benefits Brahmin priesthood (Tangled Rope)?',
    'Comparative analysis: do non-Vedic religious traditions (Christianity, Islam, Judaism, Buddhism) exhibit analogous kernel-commentary structures with analogous authority extraction? If universal: suggests natural law. If specific to Brahminical system: suggests contingency. Counterfactual: could Vedic religion function without Brahmin interpretive monopoly (parallel to Protestant direct textual access)?',
    'If natural law: constraint classification shifts toward Mountain for analytical perspective, reducing the engine''s capacity to detect extraction. If contingent: analytical perspective should classify as Tangled Rope, making extraction visible. False summit detection gates on beneficiary presence — this omega clarifies whether the beneficiaries (Brahmin priesthood) are incidental to a natural function or central to its maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether kernel-commentary structure is natural law or contingent institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brahmanical_commentary_authority, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcomm_tr_t0, brahmanical_commentary_authority, theater_ratio, 0, 0.52).
narrative_ontology:measurement(bcomm_tr_t5, brahmanical_commentary_authority, theater_ratio, 5, 0.6).
narrative_ontology:measurement(bcomm_tr_t10, brahmanical_commentary_authority, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(bcomm_be_t0, brahmanical_commentary_authority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bcomm_be_t5, brahmanical_commentary_authority, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bcomm_be_t10, brahmanical_commentary_authority, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brahmanical_commentary_authority, identity_coordination).
narrative_ontology:boltzmann_floor_override(brahmanical_commentary_authority, 0.12).
narrative_ontology:affects_constraint(brahmanical_commentary_authority, vedic_ritual_efficacy_claims).
narrative_ontology:affects_constraint(brahmanical_commentary_authority, brahmin_ritual_gatekeeping).
narrative_ontology:affects_constraint(brahmanical_commentary_authority, heterodox_delegitimization_mechanism).

% DUAL FORMULATION NOTE:
% The brahmanical commentary authority constraint decomposes into three related constraints: (1) the kernel-commentary structure itself (this story, ε=0.52, tangled_rope), (2) the ritual gatekeeping mechanism that restricts interpretive authority to those born Brahmin (higher ε, more snare-like), and (3) the delegitimization mechanism that suppresses heterodox interpretation (higher suppression, more piton-like). Each has distinct time horizons and failure modes. The core constraint affects both downstream stories through the shared kernel-commentary framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
