% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Protection Subordinate to Human Dignity (Dignity-Reading)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   The dignity-reading of the speech_harm_boundary kernel prioritizes human
 *   dignity as a foundational constraint that overrides expressive liberty
 *   when speech violates the personhood-status of target groups. This reading
 *   treats personhood-denying speech (Holocaust denial, systematic group
 *   defamation, dehumanizing hate speech) as categorically unprotected — not
 *   subject to harm-balancing tests or absolutist exception claims, but
 *   excluded entirely as incompatible with legitimate political order. The
 *   constraint exhibits Tangled Rope structure at the core: it provides
 *   genuine coordination (bright-line categorical boundaries reduce
 *   uncertainty about what speech is permissible) while imposing asymmetric
 *   extraction (speakers of identity-harm speech bear heavy restriction that
 *   other speakers do not). The extractiveness trajectory (0.42 → 0.68)
 *   reflects accumulating institutional entrenchment: as dignity-based speech
 *   codes mature and enforcement mechanisms strengthen, the extraction
 *   becomes more systematic. The suppression requirement trajectory (0.52 →
 *   0.72) reflects tightening of the boundary — what counts as
 *   dignity-violating has expanded over the measurement interval, requiring
 *   heavier suppressive apparatus. The low theater ratio (0.35) indicates
 *   that dignity-based exclusion is functionally motivated (protecting actual
 *   social goods) rather than performative, contrasting sharply with the
 *   verification_bottleneck example (0.72 theater). This reading instantiates
 *   one pole of a contested kernel: while the absolutist reading sees all
 *   suppression as illegitimate and the harm-balancing reading treats each
 *   case contextually, the dignity reading establishes categorical exclusions
 *   as a matter of foundational principle.
 *
 * KEY AGENTS:
 *   - Dignity-Protected Groups: Primary victims of identity-harm speech (powerless/trapped) — bear extraction in the form of ongoing threat to personhood-status; benefit from constraint's protective function; no exit option from jurisdiction or from group identity
 *   - Speakers of Identity-Harm Speech: Primary bearers of restriction (powerful/constrained) — face categorical prohibition on certain speech; experience constraint as asymmetric extraction despite potential coordination benefit; can exit via jurisdiction change or self-censoring at biographical cost
 *   - State Enforcement Authority: Institutional beneficiary (institutional/arbitrage) — derives legitimacy and social order benefit from dignity-boundary enforcement; sees constraint as coordination mechanism; has institutional capacity to define boundaries and enforce selectively
 *   - Expressive Liberty Coalition: Organized critics (organized/constrained) — see constraint as asymmetric restriction disguised as dignity protection; face resource and institutional pressure opposing enforcement; benefit from clarity of rules but oppose the particular rules chosen
 *   - Dignity Commons (Collective): Abstract beneficiary (powerless/trapped) — collective dignity of marginalized communities cannot organize or exit; benefits from protection but cannot negotiate the terms
 *   - International Human Rights Framework: Nominal authority (institutional/arbitrage) — textually committed to both dignity and expression; enforcement mechanisms largely theatrical in practice; benefits from dignity-protection normative commitment while lacking teeth for implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Protection Subordinate to Human Dignity (Dignity-Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, 'speech_harm_boundary__dignity_reading_v1').
narrative_ontology:cs_kernel_codification('speech_harm_boundary__dignity_reading_v1', formalized).
narrative_ontology:cs_authority_grounding('speech_harm_boundary__dignity_reading_v1', lineage).
narrative_ontology:cs_interpretation_layer_present('speech_harm_boundary__dignity_reading_v1').
narrative_ontology:cs_reading_relation('speech_harm_boundary__dignity_reading_v1', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('speech_harm_boundary__dignity_reading_v1', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('speech_harm_boundary__dignity_reading_v1', foundational, dignity_foundational_non_negotiable).
narrative_ontology:cs_axiom_status(dignity_foundational_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('speech_harm_boundary__dignity_reading_v1', dignity_foundational_non_negotiable, deontological).
narrative_ontology:cs_axiom('speech_harm_boundary__dignity_reading_v1', foundational, personhood_denying_speech_excludable).
narrative_ontology:cs_axiom_status(personhood_denying_speech_excludable, holdable).
narrative_ontology:cs_axiom_grounding('speech_harm_boundary__dignity_reading_v1', personhood_denying_speech_excludable, empirically_contingent).
narrative_ontology:cs_reference_frame('speech_harm_boundary__dignity_reading_v1', dignity_as_political_foundation).
narrative_ontology:cs_drift_state('speech_harm_boundary__dignity_reading_v1', contemporary_human_rights_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('speech_harm_boundary__dignity_reading_v1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_protected_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, state_enforcement_authority).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_identity_harm).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, expressive_liberty_domain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-HARM VICTIM (SNARE) — Faces dignity violation with no exit option. The speech targeting the agent's group (Holocaust denial, hate speech, group defamation) cannot be avoided; suppression of such speech is experienced as essential protection, not extraction. Maximum structural necessity perceived. Zero degrees of freedom to exit the harm's jurisdiction.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIGNITY COMMONS (SNARE) — The collective dignity of historically targeted groups cannot exit the jurisdiction where hate speech occurs. Generational temporal frame: the constraint operates across lifetime cycles, targeting identity categories that persist across generations. The constraint's suppression function protects a commons good that has no agency to protect itself.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SPEAKER FACING DIGNITY RESTRICTION (TANGLED ROPE) — Speakers who engage in identity-harm discourse face significant restriction but are not entirely trapped. The constraint provides coordination function (it establishes clear boundaries on acceptable speech) while imposing asymmetric extraction (penalties fall heavily on speakers of certain content). Exit is possible via leaving the jurisdiction or self-censoring, but both carry biographical costs. The speaker perceives both a genuine rule (coordination) and an asymmetric burden (extraction).
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AUTHORITY (ROPE) — The state perceives the dignity boundary as a coordination mechanism: it establishes clear rules for speech permissibility, reduces judicial uncertainty in hate speech cases, and consolidates state legitimacy through protection of vulnerable groups. The state benefits from the constraint's ability to suppress certain speech (maintaining social order) while experiencing minimal extraction burden itself. This is the perspective that benefits most from the constraint's enforcement infrastructure.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXPRESSIVE LIBERTY ADVOCATES (TANGLED ROPE) — Organized advocates for speech protection perceive both coordination (the constraint establishes bright-line rules) and extraction (the coordination systematically favors dignity protection over expressive liberty). This organized agent faces resource constraints and institutional pressure but can mobilize strategically. They see the constraint as a tangled hybrid: partially functional coordination, partially asymmetric power concentration in state hands.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS (PITON) — The global human rights ecosystem (ICCPR, ECHR, African Charter) nominally commits to both free expression and dignity protection, but the actual enforcement mechanisms for dignity protection have atrophied in many jurisdictions even while the commitment remains textually formalized. The international framework is largely theatrical at the enforcement level — normatively committed to dignity protection while lacking enforcement teeth in many signatory states. The piton classification reflects high theater ratio and institutionalized inertia.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, human dignity is presented as an irreducible foundation of political legitimacy: no social order can survive if it permits the systematic degradation of any group. This perspective sees the dignity boundary as reflecting immutable limits on what any legitimate political system can allow. However, this naturalization of the dignity-first ordering risks masking the historical and institutional contingency of how dignity protection is actually implemented and enforced. Engine false summit detection will assess whether this mountain classification naturalizes what is actually a constructed prioritization.
constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_harm_boundary__dignity_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, TR),
    TR >= 0.70.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The dignity reading produces significant extraction measured along the expressive liberty dimension. Speakers of identity-harm speech face categorical prohibition without context-dependent balancing — this is a high-cost restriction. However, the extraction is not maximal (not 0.85+) because: (1) the restriction is categorically defined (bright-line clarity reduces strategic gaming and selective enforcement compared to context-dependent balancing), (2) the protected interest (dignity/personhood of marginalized groups) is a genuine public good, and (3) speakers retain exit options (jurisdiction change, content self-censoring) even if costly. Suppression (0.72): High. The constraint requires active enforcement machinery — definition of what constitutes personhood-denying speech, investigation, adjudication, penalty application. The suppression trajectory reflects expanding boundary definitions over time. Suppression is high but not maximal (not 0.85+) because the bright-line categorical exclusion creates stable expectations rather than requiring continuous enforcement pressure. Once a speech category is classified as excluded, enforcement is relatively straightforward. Theater ratio (0.35): Moderate-low. The constraint's functional content (protecting dignity-status of targeted groups) is substantial, not performative. The constraint actually does what it claims — it reduces identity-harm speech in jurisdictions where enforced. Theater emerges mainly in the gap between nominal enforcement and actual judicial application (some cases escape the categorical rule via framing as political speech rather than hate speech), but the core mechanism is functionally sound, not ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   The dignity reading produces stark perspectival divergence. Dignity-protected groups see snare (maximum protection necessity, no exit); speakers of restricted speech see tangled rope (coordination benefits + extraction costs); the state sees rope (pure coordination via clear boundaries); civil liberties advocates see tangled rope (coordination + asymmetric burden on speakers); the international framework sees piton (nominal commitment with weak enforcement); the analytical observer risks seeing mountain (dignity as foundational law). The crucial gap: whether dignity-boundary restriction is a public good that outweighs expressive liberty costs (target group + state perspective) or an asymmetric power enforcement that uses dignity framing to suppress disfavored speech (speaker + liberty coalition perspective). The omega variables address which empirical and normative facts would resolve this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is complicated by identity-group asymmetry. Dignity-protected groups are beneficiaries of the protective function but face asymmetric vulnerability (they cannot exit their group identity, cannot exit the jurisdiction, cannot appeal to alternative frameworks). This produces high d (experienced extraction toward the beneficiary as vulnerability protection). Speakers of identity-harm speech are targets of suppression, but some are also institutional agents (media, political figures, academics) with institutional power and arbitrage options. This produces variable d across speaker categories. The state authority benefits from the constraint's coordination and legitimacy function while bearing minimal enforcement cost — low d (clear beneficiary position). The constraint requires differentiated directionality treatment: the dignity-protection benefit flows to the powerless (high d, high f(d), high experienced extraction) while the suppression cost flows to speakers (variable d depending on institutional position). A uniform beneficiary/victim declaration cannot capture this asymmetry, so the perspectives handle it through separate power-level declarations (powerless target vs. powerful speaker vs. institutional authority) that the engine uses to compute agent-specific d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The dignity reading resolves mandatrophy by accepting the tangled-rope classification as the accurate reading of constraint structure: it IS both coordination (bright-line categorical exclusion) and asymmetric extraction (speakers of certain content bear disproportionate cost). The mandatrophy arises from a false choice between 'pure coordination' (rope) and 'pure extraction' (snare). The dignity reading refuses that binary: dignity protection IS a legitimate coordination function (establishing clear boundaries, reducing uncertainty, enabling social order), AND the coordination IS asymmetrically costly to speakers of identity-harm. Both truths are structural facts. The constraint's high extractiveness (0.68) and suppression (0.72) confirm the tangled-rope classification — the constraint is not a rope with minor overhead but a genuine hybrid with significant asymmetry. This is the constraint's legitimacy claim: the asymmetry is justified by the public good being protected, not by extractive power accumulation. The mandatrophy is resolved once this dual structure is acknowledged rather than collapsed into a single dimension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_boundary_definition_instability,
    'What constitutes personhood-denying speech? Where is the bright line between identity-harm and permissible political criticism of group behavior or policy?',
    'Comparative constitutional law: analysis of where different jurisdictions draw the boundary (Holocaust denial laws, hate speech definitions, group defamation standards); case law evolution showing boundary shifts over time',
    'If boundary is stable and culturally universal: dignity reading is more robust. If boundary is contested and historically shifting: the constraint''s extractiveness derives from institutional power to define harm, not from protecting objective dignity. This transforms the constraint from tangled_rope toward snare (pure extraction via definitional control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_boundary_definition_instability, conceptual, 'Whether personhood-denying speech has an objective, stable definition or is contingent on institutional power to define harm').

omega_variable(
    dignity_protection_versus_majoritarian_suppression,
    'How much of the measurable suppression effect flows from protecting marginalized groups versus suppressing disfavored political speech through a dignity label?',
    'Empirical analysis: comparison of hate speech enforcement against historically marginalized groups versus enforcement against critical speech about majority groups; documentation of cases where dignity claims were used to suppress legitimate political dissent',
    'If suppression flows primarily to minority protection: dignity reading is validated (snare/tangled_rope accurate). If suppression is weaponized against disfavored majorities or dissent: the constraint is better modeled as snare with false dignity framing (pure extraction via definitional capture). Classification shifts toward pure extraction at high institutional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_protection_versus_majoritarian_suppression, empirical, 'Whether measured suppression protects vulnerable groups or suppresses disfavored speech via dignity framing').

omega_variable(
    speaker_exit_cost_asymmetry,
    'Do exit costs for speakers of identity-harm speech differ systematically from exit costs for speakers of other constrained speech?',
    'Comparative analysis: career outcomes, legal penalties, social sanctions for identity-harm speech versus political critique, investigative journalism, or other restricted speech categories; documentation of whether identity-harm speaker penalties are proportionally higher',
    'If exit costs are proportionally higher for identity-harm speakers: asymmetric extraction is real (tangled_rope confirmed). If exit costs are uniform across restricted speech: the constraint is a neutral speech boundary without extractive asymmetry (rope-like). If exit costs are lower for identity-harm speakers than for political dissent: the dignity framing is a false label covering political suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_exit_cost_asymmetry, empirical, 'Whether exit costs for identity-harm speakers are asymmetrically high compared to other restricted speech').

omega_variable(
    reading_contest_foreclosure_risk,
    'Does the dignity-reading''s prioritization of dignity protection logically foreclose the absolutist reading (zero suppression, maximum expression protection), or do both readings remain live policy positions?',
    'Normative analysis: can a single political framework hold both ''dignity is foundational and non-negotiable'' AND ''expressive liberty is absolute and overrides dignity claims''? Or does adoption of one reading entail rejection of the other''s core premise?',
    'If foreclosure is real: dignity reading and absolutist reading cannot coexist in the same framework; one is logically dominant (relation: forecloses). If both can coexist: they remain live competing positions across different policy communities (relation: coexists_with). This determines the reading_relations mapping in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_risk, conceptual, 'Whether dignity-first and absolute-expression readings logically foreclose each other or remain live alternatives').

omega_variable(
    dignitary_harm_empirical_reality,
    'Does exposure to personhood-denying speech cause measurable dignitary harm (psychological, social, epistemic) to target groups, or is the harm primarily a normative claim about what dignity requires?',
    'Empirical psychology/sociology: documentation of measurable harms from hate speech exposure (stress, isolation, silencing, epistemic marginalization); comparison with baseline harms from other speech restrictions; analysis of whether harm is intrinsic to speech content or depends on social context and enforcement of alternative norms',
    'If dignitary harm is empirically measurable and significant: the constraint''s extraction (suppression) is justified by real protective function. If harm is primarily normative (dignity-as-status-right rather than empirical injury): the constraint is more accurately framed as status-based restriction (potentially unequal restriction on speakers). Classification accuracy improves with empirical harm data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignitary_harm_empirical_reality, empirical, 'Whether personhood-denying speech causes empirically measurable dignitary harm').

omega_variable(
    kernel_reading_commission_effect,
    'This constraint is one reading of the ''speech_harm_boundary'' kernel. What structural features of THIS reading (dignity-first) distinguish it from the absolutist reading (zero harm justifies suppression) and harm-balancing reading (context-dependent protection)?',
    'Structural comparison: explicit statement of what premises define the dignity reading (human dignity is foundational, non-negotiable, overrides expression claims) versus what premises define siblings (absolutes never yield to context, or context always governs); documentation of which axioms are foundational to each reading and where they diverge',
    'Clarifies the committer structure: this story generates only the dignity reading. Sibling readings (absolutist, harm-balancing) are other constraint stories with their own ε values, axioms, and perspectives. The omega documents why this reading''s classification differs from siblings and anchors the kernel_context in commentary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commission_effect, conceptual, 'Kernel reading decomposition: dignity-first reading vs. absolutist and harm-balancing readings of the speech_harm_boundary kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sphd_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sphd_tr_t5, speech_harm_boundary__dignity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sphd_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sphd_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sphd_be_t5, speech_harm_boundary__dignity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(sphd_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sphd_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(sphd_su_t5, speech_harm_boundary__dignity_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(sphd_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, hate_speech_definitional_power).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, expressive_liberty_commons).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel decomposes into three structurally distinct constraint stories: dignity_reading (this file), absolutist_reading, and harm_balancing_reading. Each story has its own ε, its own beneficiary/victim structure, and its own classification profile. The dignity reading treats categorical personhood-denying speech as unprotected (high extraction, high suppression). The absolutist reading sees all suppression as delegitimizing (low extraction, low suppression of speech itself but high suppression of state authority). The harm-balancing reading treats each case contextually (medium extraction, medium suppression). Each is a coherent reading of the shared kernel; they coexist across different constitutional traditions and policy communities. The three stories are linked by network.affects_constraints to enable contamination analysis: shifts in one reading's enforcement machinery affect how aggressively the others can operate in the same jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
