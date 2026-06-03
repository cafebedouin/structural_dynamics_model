% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Categorical Near-Immunity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The absolutist reading of the speech protection kernel stakes the
 *   position that speech protection operates near-categorically, with
 *   listener harm categorically excluded as grounds for restriction. This
 *   constraint represents one coherent reading of a deeply contested kernel —
 *   the foundational commitment to what 'free speech' means in constitutional
 *   law. The reading maximizes speaker autonomy by rejecting dignitary,
 *   group-subordination, and harm-based limitations. It operates through
 *   institutional enforcement (courts striking down speech restrictions) and
 *   active suppression (rejection of victim harm claims). The constraint
 *   exhibits tangled dynamics: it genuinely coordinates speaker autonomy
 *   (beneficiaries experience rope-like coordination) while simultaneously
 *   extracting from targeted listeners and subordinated groups who suffer
 *   speech harms without legal remedy (victims experience snare-like
 *   powerlessness). The measurement trajectory shows rising extractiveness
 *   and theater ratio over 30 years, reflecting accumulating tension between
 *   the absolutist doctrine's stated coordination purpose (enabling diverse
 *   speech) and its actual function (protecting concentrated speaker power
 *   while silencing marginalized voices through organized harassment and
 *   subordinating speech). The state, institutionally bound by constitutional
 *   duty to enforce the absolutist reading, experiences the deepest
 *   tangled-rope dynamics: it must simultaneously protect both speaker
 *   autonomy and listener dignity—an institutional mandate that the
 *   absolutist reading collapses into a categorical choice favoring speakers.
 *
 * KEY AGENTS:
 *   - Speakers (Powerful, Arbitrage Exit): Institutional and high-status speakers who experience the absolutist reading as pure coordination—maximum protection, no liability. Beneficiaries.
 *   - Targeted Listeners (Powerless, Trapped): Individuals targeted by absolutist-protected harassment, slurs, coordinated doxing, and subordinating speech. Victims with no exit and no legal recourse for dignitary harm.
 *   - Subordinated Groups (Organized, Constrained): Marginalized communities organized for political expression but facing extraction through dominant-group speech protected by the absolutist doctrine. Constrained by political and legal costs of challenging the framework.
 *   - The State (Institutional, Constrained): Constitutional enforcer of the absolutist reading; institutionally trapped between competing mandates (protect speaker autonomy AND protect listener dignity) that the absolutist reading forecloses.
 *   - The Absolutist Doctrine (Institutional Inertia): The institutional framework itself, functioning as a piton—maintained through commitment to categorical principle despite observed failure to serve coordination purposes.
 *   - The Analytical Observer (Civilizational, Universal): Risk of naturalizing the absolutist reading as logically necessary rather than as a contingent institutional choice with available alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.48).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection (Categorical Near-Immunity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '558b7e0c-8c18-4a3d-bc7f-6a2df8837d33').
narrative_ontology:cs_kernel_codification('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', formalized).
narrative_ontology:cs_authority_grounding('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', lineage).
narrative_ontology:cs_interpretation_layer_present('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33').
narrative_ontology:cs_reading_relation('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_axiom('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', foundational, speaker_autonomy_categorical_override).
narrative_ontology:cs_axiom_status(speaker_autonomy_categorical_override, holdable).
narrative_ontology:cs_axiom_grounding('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', speaker_autonomy_categorical_override, deontological).
narrative_ontology:cs_axiom('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', foundational, restriction_paradox_logical_necessity).
narrative_ontology:cs_axiom_status(restriction_paradox_logical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', restriction_paradox_logical_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', categorical_speaker_autonomy).
narrative_ontology:cs_drift_state('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('558b7e0c-8c18-4a3d-bc7f-6a2df8837d33', '2026-02-26T14:32:51Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_with_protected_expression).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, institutional_speech_bearers).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targeted_listeners_suffering_dignitary_harm).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, subordinated_groups_facing_structural_harm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPEAKER (ROPE) — Powerful speakers with institutional platforms or high-status social position experience this constraint as pure coordination: the absolutist reading maximizes their communicative freedom and protects them from liability. Arbitrage exit available (can relocate discourse, amplify through multiple channels). Experiences the constraint as beneficial coordination of listener accountability to speaker autonomy. Sees no extractive cost to themselves.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: TARGETED LISTENER / VICTIM GROUP (SNARE) — Powerless agents targeted by absolutist-protected speech (harassment, slurs, coordinated doxing, subordinating speech) experience maximum extraction without exit. The absolutist reading categorically rejects dignitary harm as grounds for restriction. Trapped by geographic/social proximity to the speech and by institutional refusal to recognize their harm claim. No exit from the speech environment without complete relocation or social isolation.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBORDINATED GROUP (TANGLED ROPE) — Marginalized groups organized for political speech and advocacy face tangled dynamics: they benefit from absolutist protection when deploying their own speech for contestation and redress, but face extraction through dominant-group speech that functions as structural subordination. Constrained by political and legal costs of challenging the absolutist framework. Must navigate the paradox that the same rule protects their voice and amplifies their subordination.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE / INSTITUTIONAL ENFORCEMENT (TANGLED ROPE) — The state is institutionally constrained by constitutional duty to enforce the absolutist reading (near-categorical protection), yet also bears responsibility for protecting citizens from structural harm. Constrained exit: the state cannot simply revise the constraint without triggering institutional collapse of the constitutional order it purports to serve. Experiences genuine tension between coordination (protecting diverse speakers) and extraction (inability to remedy dignitary/structural harm). This perspectival conflict is the core of the mandate-atrophy problem.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ABSOLUTIST DOCTRINE AS INSTITUTIONAL PITON (PITON) — From the civilizational view, the absolutist reading functions as a degraded constraint maintained through institutional inertia and performative commitment to 'free speech principle' despite observed failure to serve its coordination purpose (truth discovery, democratic self-governance, speaker autonomy for marginalized voices). The doctrine persists because alternatives have not fully institutionalized, and because the functional failure is obscured by ideological commitment to the categorical principle. Theater ratio (0.35) reflects: the absolutist framework narrates itself as principled protection but functions increasingly as cover story for concentrated speaker power.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From civilizational universal scope, the absolutist reading naturalizes the protection as a logical necessity: 'speech restriction is conceptually impossible because the authority enforcing the restriction would itself be restricting speech; therefore the constraint is self-refuting and the only coherent position is near-categoricalism.' This perspective sees the absolutist reading as emerging naturally from the logical structure of any free-speech regime. However, this view fails to account for structural data (organized beneficiaries, powerless victims, active enforcement suppression mechanisms), making it a false summit candidate.
constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_kernel__absolutist_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high, reflecting the tangled dynamics. The absolutist reading genuinely coordinates some speaker autonomy (beneficiaries experience protection without limitation), but asymmetrically — powerful speakers benefit disproportionately, while the powerless (who most need protection from harassment) gain protection that yields no practical autonomy. The extraction emerges from the state's inability to remedy dignitary and subordination harms while enforcing the categorical doctrine. Base extractiveness has risen from 0.48 to 0.62 over 30 years, tracking accumulating tension as digital communication enabled scalable harassment and coordinated subordination (organized doxing, algorithmic amplification of dehumanizing speech, targeted harassment campaigns) that the absolutist framework cannot address. Suppression (0.48): Moderate. The constraint operates through suppression of victim harm claims (courts reject listener harm as grounds for restriction) and through the chilling effect on marginalized speakers who fear retaliation from absolutist-protected harassment. Rising from 0.38 to 0.48, reflecting hardened categorical rejection of harm claims as digital-era harms (coordinated harassment, subordinating at-scale speech) accumulate. Theater ratio (0.35): Moderate-low, rising. The absolutist doctrine narrates itself as principled protection of free speech, but increasingly functions as cover for concentrated speaker power. The theater reflects: (1) performative invocation of 'free speech principle' to justify protection of speech that demonstrably silences subordinated speakers; (2) ideological commitment to logical necessity ('restriction is self-refuting') obscuring empirical evidence that threshold-based systems in other jurisdictions function coherently; (3) institutional persistence through categorical commitment despite observed failure to serve stated coordination purposes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Powerful speakers see rope (pure coordination). Powerless victims see snare (categorical extraction). Subordinated groups see tangled rope (paradoxical: the rule protects them and harms them simultaneously). The state sees the deepest tangled rope (institutionally trapped between two categorical mandates). The institutional doctrine itself functions as a piton (degraded ritual maintained through inertia). The analytical observer risks mountain classification (logical necessity) that the structural data contradicts. The perspectival gap is not failure of the analytical framework—it is the accurate diagnosis. The absolutist reading genuinely produces different classification from different structural positions. The question is not which classification is 'correct,' but whether the absolutist reading's institutional dominance forecloses the legitimate perspectives (victim-centered, subordination-recognizing) that other readings of the kernel would enable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Powerful speakers with arbitrage exit (can amplify through multiple channels, relocate discourse, access institutional platforms) are net beneficiaries, deriving low d values. Powerless trapped victims with no exit derive high d values (0.9+), experiencing maximum extracted force. Organized subordinated groups with constrained exit occupy the middle (0.55-0.75). The state's institutional constraint (cannot exit without constitutional collapse) produces moderate d (~0.50) despite institutional power, because the constraint forces incompatible mandates. The tangled-rope classification emerges from the asymmetry: beneficiaries experience coordination (chi near baseline), victims experience extraction (chi amplified by f(d)). The false summit risk arises from the analytical observer's perspective, which risks deriving d from the logical structure of speech restriction ('the authority cannot restrict itself') rather than from the empirical structure of who benefits and who is harmed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint instantiates the core mandatrophy of liberal constitutionalism: the state is mandated by the absolutist reading to maximize speaker autonomy categorically, yet also mandated by its basic function to protect citizens from subordination and dignitary harm. The absolutist reading collapses this into a categorical choice (protect speakers; reject harm claims). The measurement trajectory shows accumulating strain: base extractiveness rises as digital-era harms (scalable harassment, algorithmic subordination, coordinated doxing) outpace the judicial system's willingness to recognize them under the categorical doctrine. Suppression rises as victim claims are more actively rejected. Theater rises as the doctrine's cover-story function becomes more visible—it narrates itself as principled free-speech protection while functioning as concentrated-speaker protection. The mandatrophy is not resolvable by choosing a different classification—it is resolvable only by recognizing that the absolutist reading, while coherent as a logical proposition ('near-categorical protection'), produces observable failures of its stated coordination function (enabling diverse speech, protecting speaker autonomy for marginalized voices, discovering truth through marketplace competition). The resolution mechanism is not 'which type is right?' but 'should the institutional framework adopt a different reading of the kernel that can accommodate both speaker autonomy AND listener/group dignity?' This is a political question, not a mathematical one, and the constraint story documents why the political choice matters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_threshold_coherence,
    'Is near-categorical speech protection logically necessary (self-refuting to restrict the restriction authority''s own speech) or is it a contingent institutional choice with available threshold-based alternatives?',
    'Comparative constitutional analysis: jurisdictions with threshold-based or dignity-based speech restrictions that function coherently (Canada, Germany, EU nations) demonstrate that categorical protection is not logically required. The ''self-refuting restriction'' argument proves only that speech-about-speech-restriction cannot itself be restricted without paradox — not that substantive speech harms cannot be regulated.',
    'If categorical protection is logically necessary: mountain classification confirmed; no alternatives exist. If threshold-based alternatives are coherent: the absolutist reading is a contingent institutional choice; false summit detection applies; beneficiary presence suggests the categorical frame naturalizes what is contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_threshold_coherence, conceptual, 'Whether categorical speech protection is logically necessary or contingent institutional choice').

omega_variable(
    harm_measurement_ambiguity,
    'What counts as ''listener harm'' that might ground restriction under a threshold-based reading? Emotional distress? Diminished participation in public discourse? Reinforcement of structural subordination? Economic injury? Reputational damage?',
    'Specification of harm categories and empirical measurement protocols. Different threshold-based readings (dignity_reading, harm_threshold_reading) use different harm definitions. The absolutist reading categorically rejects all harm claims as grounds. Empirical evidence on which harm categories predict measurable outcomes (chilling effect, group-silencing, economic coercion) would inform whether the absolutist rejection is over-broad.',
    'If concrete harms (measurable silencing, coordinated harassment, economic coercion) cannot be distinguished from speculative harms (offense, disagreement), the absolutist reading''s categorical rejection may be justified. If harms are measurable and causally linked to speech regulation capacity, the absolutist reading suppresses legitimate victim claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_measurement_ambiguity, empirical, 'Definition and measurability of listener harm categories relevant to speech restriction').

omega_variable(
    sibling_reading_interaction,
    'How does the absolutist reading''s logical structure relate to the other four readings of the speech_protection_kernel? Do they coexist in legal practice, or does the absolutist reading foreclose alternatives within the same institutional framework?',
    'Historical and comparative legal analysis. U.S. law predominantly instantiates the absolutist reading (with narrow categorical exclusions). EU/Canadian law instantiates dignity and harm-threshold readings. Are these readings coexisting across different jurisdictions, or is the absolutist reading''s institutional dominance in the U.S. foreclosing alternatives within American constitutional doctrine?',
    'If coexisting: multiple readings remain live in global institutional practice. If the absolutist reading forecloses alternatives in U.S. constitutional law, the reading_relations should mark foreclosure rather than coexistence. This omega documents whether the kernel contest is genuinely open or already resolved within specific institutional spheres.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_interaction, conceptual, 'Logical and institutional relationships between absolutist and alternative speech protection readings').

omega_variable(
    speaker_power_asymmetry,
    'Does near-categorical speech protection serve the absolutist reading''s stated purpose (maximizing speaker autonomy) equally for all speakers, or does it disproportionately amplify powerful speakers and silence powerless ones?',
    'Empirical analysis of speech reach and amplification by power level; measurement of differential chilling effects (powerful speakers suffer fewer legal threats than marginalized speakers); comparative liability risk across speaker categories. If amplification is asymmetric, the absolutist reading''s coordination function (speaker autonomy) is partially illusory — it functions as extraction for powerful speakers (protection + megaphone) and constraint for powerless ones (protection that doesn''t help if you have no platform).',
    'If speaker autonomy is equally distributed: coordination function confirmed; tangled_rope classification justified but toward rope end. If asymmetric: coordination function is captured; the constraint is closer to pure snare for powerless speakers; mandatrophy deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speaker_power_asymmetry, empirical, 'Asymmetry in speech amplification and chilling effects across speaker power levels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_abs_theater_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(spk_abs_theater_t15, speech_protection_kernel__absolutist_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(spk_abs_theater_t30, speech_protection_kernel__absolutist_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(spk_abs_extract_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spk_abs_extract_t15, speech_protection_kernel__absolutist_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(spk_abs_extract_t30, speech_protection_kernel__absolutist_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spk_abs_suppress_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(spk_abs_suppress_t15, speech_protection_kernel__absolutist_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(spk_abs_suppress_t30, speech_protection_kernel__absolutist_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, digital_platform_speech_amplification).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, subordinating_speech_structural_effects).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel constraint family consists of five readings instantiating different core premises about what speech protection fundamentally requires. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification. They are linked through network.affects_constraints rather than combined into one constraint, per ε-invariance principle (DP-001). The absolutist_reading has ε=0.62 (tangled_rope, coordination + extraction hybrid). Alternative readings will have different ε values reflecting their different empirical assumptions about coordination function, harm measurability, and institutional capacity. No single reading is 'the' speech protection constraint; the kernel contest is the structure that matters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
