% ============================================================================
% CONSTRAINT STORY: parliamentary_supremacy_statutes__fixed_term_parliaments_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliamentary_supremacy_statutes__fixed_term_parliaments_act, []).

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
 *   constraint_id: parliamentary_supremacy_statutes__fixed_term_parliaments_act
 *   human_readable: Fixed-term Parliaments Act: Statutized and Revived Electoral Timing Control
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Fixed-term Parliaments Act (2011–2017) provides a stark case of
 *   constitutional supremacy demonstrated via round-trip: Parliament
 *   statutized the Prime Minister's dissolution power, effectively removing
 *   the Crown's prerogative discretion to call elections at strategic
 *   moments. Six years later, the statute was repealed with minimal
 *   procedural resistance, and the prerogative was restored. This cycle
 *   reveals the fundamental asymmetry: Parliament demonstrated its supremacy
 *   by imposing the statute AND by un-imposing it, showing that no constraint
 *   on the Crown's powers is permanent against parliamentary will. Yet the
 *   cycle also illustrates the extractive power of electoral timing: whoever
 *   controls dissolution timing extracts significant political advantages.
 *   The constraint combines coordination (fixed terms enable stable
 *   legislative planning) with asymmetric extraction (discretionary timing
 *   benefits the executive). The round-trip itself proves Parliament's
 *   supremacy, but masks the fact that the executive is the primary
 *   beneficiary of returning to discretionary dissolution.
 *
 * KEY AGENTS:
 *   - Prime Minister / Executive: Primary beneficiary (institutional/arbitrage) — regains unilateral control over election timing; can call elections at strategically advantageous moments
 *   - Opposition Parties: Primary victim under pre-statute and post-repeal regimes (powerless/trapped) — cannot call elections; subject to executive's timing choices; extract disadvantage from unpredictable dissolution
 *   - Opposition Parties Under Statute: Secondary beneficiary (organized/constrained) — gain predictability and removal of dissolution weapon; lose ability to capitalize on momentum
 *   - Parliament (as supremacy claimant): Beneficiary of the round-trip demonstration (institutional/analytical) — uses repeal as evidence of fundamental supremacy over prerogative
 *   - Statutory Framework (during 2011–2017): Victim of repeal (institutional/constrained) — demonstrates that statutes constraining prerogative can be reversed; structural impermanence proves Parliament's supremacy but not the statute's durability
 *   - Electoral Timing Losers (post-repeal): Primary victim (powerless/trapped) — face extractive timing control by executive; must respond to incumbent-favored election calls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliamentary_supremacy_statutes__fixed_term_parliaments_act, 0.58).
domain_priors:suppression_score(parliamentary_supremacy_statutes__fixed_term_parliaments_act, 0.62).
domain_priors:theater_ratio(parliamentary_supremacy_statutes__fixed_term_parliaments_act, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__fixed_term_parliaments_act, extractiveness, 0.58).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__fixed_term_parliaments_act, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__fixed_term_parliaments_act, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliamentary_supremacy_statutes__fixed_term_parliaments_act, tangled_rope).
narrative_ontology:human_readable(parliamentary_supremacy_statutes__fixed_term_parliaments_act, "Fixed-term Parliaments Act: Statutized and Revived Electoral Timing Control").
narrative_ontology:topic_domain(parliamentary_supremacy_statutes__fixed_term_parliaments_act, "political/constitutional").

domain_priors:requires_active_enforcement(parliamentary_supremacy_statutes__fixed_term_parliaments_act).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliamentary_supremacy_statutes__fixed_term_parliaments_act, '68db3337-3129-4f67-b218-266a57cfdb5c').
narrative_ontology:cs_kernel_codification('68db3337-3129-4f67-b218-266a57cfdb5c', formalized).
narrative_ontology:cs_authority_grounding('68db3337-3129-4f67-b218-266a57cfdb5c', lineage).
narrative_ontology:cs_interpretation_layer_present('68db3337-3129-4f67-b218-266a57cfdb5c').
narrative_ontology:cs_reading_relation('68db3337-3129-4f67-b218-266a57cfdb5c', parliamentary_supremacy_statutes__acts_of_union, coexists_with).
narrative_ontology:cs_reading_relation('68db3337-3129-4f67-b218-266a57cfdb5c', parliamentary_supremacy_statutes__parliament_act_1911, coexists_with).
narrative_ontology:cs_reading_relation('68db3337-3129-4f67-b218-266a57cfdb5c', parliamentary_supremacy_statutes__parliament_act_1949, coexists_with).
narrative_ontology:cs_axiom('68db3337-3129-4f67-b218-266a57cfdb5c', foundational, prerogative_susceptible_statutory_constraint).
narrative_ontology:cs_axiom_status(prerogative_susceptible_statutory_constraint, holdable).
narrative_ontology:cs_axiom_grounding('68db3337-3129-4f67-b218-266a57cfdb5c', prerogative_susceptible_statutory_constraint, conventional).
narrative_ontology:cs_axiom('68db3337-3129-4f67-b218-266a57cfdb5c', foundational, statutory_reversal_proves_parliamentary_supremacy).
narrative_ontology:cs_axiom_status(statutory_reversal_proves_parliamentary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('68db3337-3129-4f67-b218-266a57cfdb5c', statutory_reversal_proves_parliamentary_supremacy, deontological).
narrative_ontology:cs_reference_frame('68db3337-3129-4f67-b218-266a57cfdb5c', crown_prerogative_dissolution_discretion).
narrative_ontology:cs_drift_state('68db3337-3129-4f67-b218-266a57cfdb5c', contemporary_post_repeal, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68db3337-3129-4f67-b218-266a57cfdb5c', '').
narrative_ontology:cs_kernel_id(parliamentary_supremacy_statutes__fixed_term_parliaments_act, parliamentary_supremacy_statutes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__fixed_term_parliaments_act, opposition_parties).
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__fixed_term_parliaments_act, prime_minister).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__fixed_term_parliaments_act, electoral_predictability_losers).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__fixed_term_parliaments_act, statutory_constraint_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPPOSITION PARTIES UNDER STATUTE (SNARE) — Fixed terms removed the incumbent PM's ability to call elections at strategic moments. Opposition parties initially experienced this as relief (predictability, no surprise dissolution). But the statute became a trap: it also prevented them from calling elections when they had momentum. High suppression of electoral agency; extraction flows toward whoever controls legislative time-allocation during the fixed term. The opposition could not escape the statutory framework without securing majority coalition support.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT PM UNDER STATUTE (ROPE) — Fixed terms as coordination: the PM coordinates parliamentary business without fear of sudden dissolution. The constraint removes a pressure tool but also a liability. Moderate extraction because the PM benefits from legislative predictability while losing timing flexibility. This is experienced as a coordination mechanism — the statute creates stable expectations for legislative planning.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTI-PARTY COALITION (TANGLED ROPE) — From a coalition perspective, fixed terms create genuine coordination (all parties can plan) alongside asymmetric extraction (timing flexibility extracted from the executive). The statute required cooperation to repeal it, creating a coordination function while asymmetrically constraining the executive. Suppression is high (no unilateral exit), but both coordination and extraction are present and real.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CROWN PREROGATIVE REASSERTED (MOUNTAIN) — From the Crown's structural position, the repeal of the Fixed-term Parliaments Act appears to restore an immutable constitutional reality: the Crown's prerogative power to dissolve Parliament is a foundational element of parliamentary sovereignty that no statute can permanently strip away. The brief statutory interlude (2011–2017) is read as a contingent aberration; the prerogative's return appears as an inevitable restoration of natural constitutional law. However, this perspective may constitute a false summit: the prerogative's reinstatement required deliberate political choice and statutory repeal, not an automatic constitutional restoration.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE STATUTE AS DEGRADED ARTIFACT (PITON) — The Fixed-term Parliaments Act persisted from 2011–2017 as institutional theater: it was widely recognized as awkward (early elections required supermajority votes to dissolve, creating procedural complexity). The statute remained in force not because it functioned well but because repealing it required political capital. It was eventually repealed with minimal resistance, suggesting low structural commitment to its functions. Theater ratio reflects its performative maintenance despite acknowledged dysfunction.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL TIMING LOSERS (POST-REPEAL) — After repeal, the Crown's prerogative power over dissolution timing returned fully to the executive. Parties now face extractive timing decisions: the incumbent can call elections at strategically advantageous moments. Those without executive power experience maximum suppression — they cannot call elections and must respond to incumbent timing. This is the snare experience: high extraction, high suppression, minimal coordination benefit.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL SUPREMACY (TANGLED ROPE) — From a civilizational analytical perspective, the round-trip (statute → repeal → prerogative restored) demonstrates Parliament's supremacy over the Crown's prerogative. The constraint is neither immutable law nor pure extraction but a structural feature of parliamentary sovereignty: Parliament can statutize prerogatives, then un-statutize them, proving its fundamental supremacy. The round-trip itself is the evidence. But this reading obscures the extractive power of electoral timing: regardless of formal supremacy, whoever controls the dissolution power extracts timing advantages. The constraint combines coordination (legislative predictability under statute) with asymmetric extraction (timing control post-repeal).
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliamentary_supremacy_statutes__fixed_term_parliaments_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__fixed_term_parliaments_act, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parliamentary_supremacy_statutes__fixed_term_parliaments_act, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parliamentary_supremacy_statutes__fixed_term_parliaments_act, TR),
    TR >= 0.70.

:- end_tests(parliamentary_supremacy_statutes__fixed_term_parliaments_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness trajectory: Pre-statute (0.72): Discretionary dissolution is highly extractive — the executive unilaterally determines electoral timing, creating significant uncertainty and timing disadvantage for opposition parties. Mid-statute (0.35): Fixed terms remove the timing tool; extractiveness drops sharply because the executive's timing flexibility is statutized away. Coordination benefits emerge (predictable parliamentary terms). Post-repeal (0.58): Extractiveness rises again as discretionary dissolution returns, but not to pre-statute levels because the statutory round-trip has created political precedent and transparency around the extraction mechanism. Suppression trajectory: Pre-statute (0.68): Opposition parties face high suppression — they cannot unilaterally call elections and must respond to executive timing. Mid-statute (0.50): Suppression decreases under fixed terms — all parties know when elections will occur; uncertainty is removed. Post-repeal (0.62): Suppression returns to near pre-statute levels as executive timing discretion is restored. Theater ratio: Pre-statute (0.55): Moderate theater — dissolution procedures are procedurally formal but substantively discretionary. Mid-statute (0.42): Lower theater during statute because fixed terms remove performative discretion; elections occur by schedule, not executive choreography. Post-repeal (0.45): Theater remains relatively low because the repeal itself was straightforward and the prerogative's restoration was explicit, not dressed in constitutional mystification. The round-trip's effect is to lower theater: both the statute and its repeal are transparent exercises of parliamentary power.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The opposition parties trapped under discretionary dissolution see Snare (extraction, no exit). Under the statute, they see modest benefit but also constraint (Rope during statute, because the coordination benefit is genuine but they retain constrained agency). The incumbent PM under the statute sees Rope (coordination without dissolution timing flexibility is still workable). After repeal, the executive sees Rope (efficient coordination restored, timing flexibility regained). The opposition sees Snare (trapped timing vulnerability returns). The powerful/mobile/analytical perspective seeing the prerogative's reinstatement classifies as Mountain (natural law restored) — but this risks false summit status because the prerogative's restoration required deliberate political choice and statutory repeal, suggesting it is not immutable but constructed and beneficiary-serving. The multi-party coalition perspective sees Tangled Rope (coordination built into the statute, asymmetric extraction in discretionary dissolution). The statute-as-artifact perspective sees Piton (institutional performance without durable function; easily repealed once political will aligned).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: The Prime Minister and executive apparatus benefit from discretionary dissolution power and thus have low d (around 0.20–0.25 for arbitrage/institutional context). Under the statute, opposition parties gain predictability but lose timing flexibility — they are neither pure beneficiaries nor pure victims; their d under statute is around 0.50 (symmetric). Post-repeal, opposition parties become victims of executive timing choices; d shifts to 0.75–0.85 (constrained/trapped depending on organization). The statutory framework itself (as a constraint to be maintained or repealed) has no agent directly benefiting from its persistence; its d is around 0.70 (victim of repeal). The false-summit perspective (prerogative as Mountain) assigns the prerogative to an analytical agent with d around 0.73; the FSM flag would suggest reclassifying to Tangled Rope (coordination + extraction) once beneficiaries are recognized.
 *
 * MANDATROPHY ANALYSIS:
 *   The FTPA constraint demonstrates mandatrophy resolution through perspectival cycling: pre-statute perspectives converge on Snare/extraction narrative. Statute introduces Rope/coordination reading. Post-repeal, perspectives fragment again into Snare/Rope/Tangled Rope depending on agent position. The round-trip itself resolves mandatrophy by showing that both extraction and coordination narratives are legitimate perspectival readings: the same structural mechanism (dissolution power) appears as extractive timing control from opposition perspective, as coordination framework from coalition perspective, and as constitutional supremacy demonstration from analytical perspective. No single type is 'true' — the presheaf over the observation site (pre-statute, during statute, post-repeal) shows that the constraint's classification is observer-relative and time-dependent. The mandatrophy resolves by accepting multiperspectival validity rather than seeking a single terminal classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_prerogative_hierarchy,
    'Can a statute permanently constrain a prerogative power, or does the prerogative''s constitutional status make it unsusceptible to statutory suppression?',
    'Jurisprudential test: examine whether any statute successfully constrains prerogatives indefinitely, or whether prerogatives persist in reasserting themselves through political repeal of constraining legislation. The FTPA round-trip is evidence but not dispositive — it shows prerogatives CAN be temporarily statutized AND repealed, but not whether this represents permanent supremacy of statute or structural dominance of prerogative.',
    'If statute can permanently constrain prerogative: parliamentary supremacy is absolute, and the FTPA repeal was a political choice, not a constitutional restoration. Classification shifts toward Rope (coordination via statute) with lower structural extraction. If prerogatives are structurally dominant: the FTPA was a temporary aberration; the prerogative''s return is inevitable; post-repeal extraction is constitutionally natural. Classification shifts toward Mountain (or false-summit with FSM flag).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_prerogative_hierarchy, conceptual, 'Whether statute can permanently constrain prerogative power').

omega_variable(
    electoral_timing_extraction_quantification,
    'How much electoral advantage does the ability to call elections at strategically optimal moments (incumbency advantage) extract from opposition parties across a typical electoral cycle?',
    'Empirical analysis: compare electoral outcomes (incumbent party margin, seat gains/losses, third-party performance) under fixed-term regimes (statute period 2011–2017) vs. discretionary dissolution regimes (pre-2011 and post-2017). Quantify whether incumbents'' ability to time elections correlates with higher winning margins or seat preservation.',
    'If timing advantage is quantifiable and significant (>2–5% seat swing): extractiveness justified at 0.55+; victim classification for opposition parties is robust. If timing advantage is marginal or cancelled by counterfactors (e.g., bad political climate overrides timing advantage): extractiveness drops to 0.35–0.40, classification shifts toward Rope (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_timing_extraction_quantification, empirical, 'Magnitude of electoral advantage from dissolution timing control').

omega_variable(
    false_summit_prerogative_naturalness,
    'Is the Crown''s prerogative power over dissolution a natural law of constitutional order, or a contingent institutional arrangement that benefits the executive and thus has identifiable beneficiaries?',
    'Structural test: if identifiable beneficiaries exist (the executive, specifically the Prime Minister), the prerogative is not a natural law but a constructed constraint. The false-summit mechanism fires: the Crown''s prerogative classified as Mountain by the powerful/mobile/analytical perspective is reclassified to Tangled Rope or Snare to reflect the extractive structure. Alternatively, the prerogative''s restoration through political choice (rather than automatic constitutional restoration) suggests it is contingent, not natural.',
    'If false summit confirmed: the Mountain perspective is a naturalizing cover story. Classify as Tangled Rope (coordination + extraction) or Snare (pure extraction). If prerogative is genuinely natural: Mountain holds, and the repeal/restoration cycle is merely procedural variation atop an immutable foundation. FSM flag would be incorrect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_prerogative_naturalness, conceptual, 'Whether prerogative power is natural law or contingent institutional arrangement with beneficiaries').

omega_variable(
    reading_contest_kernel_identity,
    'Is the kernel being read here ''parliamentary supremacy over the Crown'' or ''the Crown''s prerogative power over dissolution timing''? Different readings emphasize different aspects of the same statutory round-trip.',
    'Kernel reconstruction: the FTPA is one reading of parliamentary_supremacy_statutes. Other readings (Acts of Union, Parliament Acts 1911/1949) assert different aspects of the same underlying contest: CAN Parliament permanently alter the Crown''s powers? The FTPA reading says ''yes, but only until Parliament repeals the statute'' — demonstrating supremacy through the ability to un-statutize. Sibling readings may assert supremacy through different mechanisms (explicit union, chamber limitation, self-amending procedure). The kernel is the question; each reading provides one answer.',
    'If kernel is ''parliamentary supremacy'': this reading demonstrates it via round-trip; sibling readings demonstrate it via other mechanisms; all readings coexist as evidence of the same underlying claim. If kernel is ''Crown prerogative stability'': the FTPA reading shows that prerogative can be temporarily constrained but is structurally difficult to permanently suppress; sibling readings explore similar constraints on prerogatives. Framing the kernel correctly determines which sibling relations apply (coexists_with vs forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Kernel identity and reading relationship to sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliamentary_supremacy_statutes__fixed_term_parliaments_act, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ftpa_theater_pre_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ftpa_theater_mid_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, theater_ratio, 3, 0.42).
narrative_ontology:measurement(ftpa_theater_post_repeal, parliamentary_supremacy_statutes__fixed_term_parliaments_act, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(ftpa_extractiveness_pre_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(ftpa_extractiveness_mid_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(ftpa_extractiveness_post_repeal, parliamentary_supremacy_statutes__fixed_term_parliaments_act, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ftpa_suppression_pre_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(ftpa_suppression_mid_statute, parliamentary_supremacy_statutes__fixed_term_parliaments_act, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(ftpa_suppression_post_repeal, parliamentary_supremacy_statutes__fixed_term_parliaments_act, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliamentary_supremacy_statutes__fixed_term_parliaments_act, enforcement_mechanism).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__fixed_term_parliaments_act, acts_of_union).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__fixed_term_parliaments_act, parliament_act_1911).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__fixed_term_parliaments_act, parliament_act_1949).

% DUAL FORMULATION NOTE:
% The FTPA reading is part of the parliamentary_supremacy_statutes constraint family. All four readings (FTPA, Acts of Union, 1911, 1949) instantiate the same kernel: can Parliament permanently constrain or alter the Crown's powers? Each reading uses a different historical mechanism as evidence. The FTPA reading emphasizes the round-trip (statute → repeal) as proof of supremacy. The 1911 and 1949 readings emphasize chamber limitation and procedural self-amendment. The Acts of Union reading emphasizes territorial supremacy through merger by statute. Each story has its own extractiveness value reflecting the specific constraint mechanism. Network links indicate that all readings share the same underlying constitutional question, but each provides distinct structural evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parliamentary_supremacy_statutes__fixed_term_parliaments_act, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
