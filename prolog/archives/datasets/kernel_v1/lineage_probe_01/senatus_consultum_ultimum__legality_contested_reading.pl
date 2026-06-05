% ============================================================================
% CONSTRAINT STORY: senatus_consultum_ultimum__legality_contested_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senatus_consultum_ultimum__legality_contested_reading, []).

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
 *   constraint_id: senatus_consultum_ultimum__legality_contested_reading
 *   human_readable: Senatus Consultum Ultimum: Legality Contested (Reading)
 *   domain: legal/constitutional/Roman_Republic
 *
 * SUMMARY:
 *   The Senatus Consultum Ultimum (Final Decree of the Senate) emerged as the
 *   Republic's emergency mechanism: when the state faced existential threat,
 *   the Senate could exhort the consuls to act without normal constitutional
 *   limits. Its legality was contested from its first use. Cicero executed
 *   the Catilinarian conspirators under it and was later exiled for exactly
 *   that — not tried and acquitted, but driven from the state by
 *   retrospective legal judgment that the decree had not authorized him to
 *   kill citizens without trial. The reading instantiated here treats
 *   legality as genuinely contested in the Republic's own courts and
 *   political institutions, not as a false claim hiding extra-legal power.
 *   The beneficiary of the decree (the senatorial faction in power at any
 *   moment) alternated with the political wind. The victim set expanded over
 *   time: first the executed citizens, then the executors themselves (exposed
 *   to legal liability), finally the republican legal framework (forced to
 *   adjudicate its own suspension). This reading coexists with two siblings:
 *   the emergency_without_office_reading (the decree had no defined
 *   safeguards like the dictatorship's term or task), and the
 *   gracchan_precedent_reading (the decree was born killing reformers, used
 *   as factional weapon from its inception). All three readings are live
 *   positions in the Republic's political theology — none definitively
 *   forecloses the others, though they emphasize different structural
 *   features of the same mechanism.
 *
 * KEY AGENTS:
 *   - Executed Citizens (powerless/trapped): Bear extraction via death without trial; have no agency or appeal; experience the decree as snare
 *   - Executing Consuls, esp. Cicero (powerful/constrained): Coordinate emergency response via senatorial backing (rope benefit) but later face exile when political enemies reinterpret the decree's legality (extraction cost); experience tangled rope at biographical horizon
 *   - Senatorial Faction in Power (institutional/arbitrage): Benefit from emergency authorization without needing to enforce it personally; invoke the decree when convenient, withdraw it when costly; experience rope (pure coordination benefit)
 *   - Republican Legal Tradition (identity_locked/moderate): Constitutionally committed to due process but forced to authorize its suspension via the decree; cannot abandon the claim to operate under law while suspending law; experience identity-locked tangled rope
 *   - Reformist Opposition, esp. Gracchans (organized/constrained): Face the decree as weapon of suppression (gracchan precedent reading); build organizational resistance over generations; experience constrained tangled rope at generational horizon
 *   - Imperial Successor State (institutional/arbitrage): Renders the contested legality moot by replacing the mechanism; maintains symbolic legality while ceasing functional use; experience piton (degraded ritual)
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the legality contest as an immutable feature of emergency law itself, instantiating the oracle gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senatus_consultum_ultimum__legality_contested_reading, 0.58).
domain_priors:suppression_score(senatus_consultum_ultimum__legality_contested_reading, 0.68).
domain_priors:theater_ratio(senatus_consultum_ultimum__legality_contested_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senatus_consultum_ultimum__legality_contested_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(senatus_consultum_ultimum__legality_contested_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(senatus_consultum_ultimum__legality_contested_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senatus_consultum_ultimum__legality_contested_reading, tangled_rope).
narrative_ontology:human_readable(senatus_consultum_ultimum__legality_contested_reading, "Senatus Consultum Ultimum: Legality Contested (Reading)").
narrative_ontology:topic_domain(senatus_consultum_ultimum__legality_contested_reading, "legal/constitutional/Roman_Republic").

domain_priors:requires_active_enforcement(senatus_consultum_ultimum__legality_contested_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(senatus_consultum_ultimum__legality_contested_reading, 'f819231f-e00a-46a3-bda9-a98a891304a3').
narrative_ontology:cs_kernel_codification('f819231f-e00a-46a3-bda9-a98a891304a3', formalized).
narrative_ontology:cs_authority_grounding('f819231f-e00a-46a3-bda9-a98a891304a3', extraction).
narrative_ontology:cs_interpretation_layer_present('f819231f-e00a-46a3-bda9-a98a891304a3').
narrative_ontology:cs_reading_relation('f819231f-e00a-46a3-bda9-a98a891304a3', senatus_consultum_ultimum__emergency_without_office_reading, coexists_with).
narrative_ontology:cs_reading_relation('f819231f-e00a-46a3-bda9-a98a891304a3', senatus_consultum_ultimum__gracchan_precedent_reading, coexists_with).
narrative_ontology:cs_axiom('f819231f-e00a-46a3-bda9-a98a891304a3', foundational, legal_authorization_possible_for_emergency).
narrative_ontology:cs_axiom_status(legal_authorization_possible_for_emergency, holdable).
narrative_ontology:cs_axiom_grounding('f819231f-e00a-46a3-bda9-a98a891304a3', legal_authorization_possible_for_emergency, deontological).
narrative_ontology:cs_axiom('f819231f-e00a-46a3-bda9-a98a891304a3', foundational, unresolved_legality_destabilizes_commonwealth).
narrative_ontology:cs_axiom_status(unresolved_legality_destabilizes_commonwealth, holdable).
narrative_ontology:cs_axiom_grounding('f819231f-e00a-46a3-bda9-a98a891304a3', unresolved_legality_destabilizes_commonwealth, empirically_contingent).
narrative_ontology:cs_reference_frame('f819231f-e00a-46a3-bda9-a98a891304a3', republican_due_process_authority).
narrative_ontology:cs_drift_state('f819231f-e00a-46a3-bda9-a98a891304a3', post_ciceronian_trials, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f819231f-e00a-46a3-bda9-a98a891304a3', '').
narrative_ontology:cs_kernel_id(senatus_consultum_ultimum__legality_contested_reading, senatus_consultum_ultimum).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senatus_consultum_ultimum__legality_contested_reading, senatorial_faction_in_power).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__legality_contested_reading, executed_citizens_without_trial).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__legality_contested_reading, exiled_executors).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__legality_contested_reading, republican_legal_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXECUTED CITIZEN (SNARE) — No trial, no appeal, no escape. Death decree rendered via exhortation to consuls, without legal process or defined crime. The trapped agent bears extraction — loss of life — with zero agency. Maximum suppression: the decree operates precisely by eliminating due process alternatives.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE EXECUTING CONSUL (TANGLED ROPE) — Experiences both coordination benefit (senatorial backing during crisis) and extraction (legal liability). Cicero's case exemplifies this: his execution of the Catilinarian conspirators under the SCU was legal at the moment, beneficial to the Republic under the coordination frame, yet exposed him to exile when the political wind shifted. The consul's power is real but constrained by the decree's contested legal status — he coordinates emergency response with the Senate but remains vulnerable to later prosecution.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SENATORIAL FACTION IN POWER (ROPE) — Benefits from the decree's coordination function: it legitimates extrajudicial action by consuls when the Senate faces factional threats. The faction experiences the SCU as pure coordination — 'we need emergency power to preserve the state.' The immediate horizon and arbitrage exit reflect their ability to invoke or withdraw the decree depending on political conditions. No contradiction: they benefit from using it, and face no legal exposure if they don't execute it personally.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE REPUBLICAN LEGAL TRADITION (TANGLED ROPE) — Constrained by identity fusion with due process norms. The tradition genuinely coordinates emergency response (coordination benefit) while extracting authority from its own procedural safeguards (extraction cost). The identity lock is structural: the legal tradition cannot abandon the claim that it operates under law, yet the decree operates through law's suspension. The tradition experiences this as internal contradiction — forced to adjudicate its own violation. Biographical horizon because the tradition's legitimacy is tested within individual cases.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REFORMIST OPPOSITION (TANGLED ROPE) — Constrained by the decree's power but gaining organizational capacity over generations. Experiences coordination benefit (emergency power can theoretically protect against tyranny) alongside extraction (the decree is used to suppress reform). Generational horizon reflects that opposition builds resistance over decades — Gracchan precedent creates precedent, Catilinarian case establishes judicial exposure, eventual Republic collapse reveals the mechanism's failure. Not trapped (organized agents have agency), not mobile (the decree's legal ambiguity prevents clean exit), not arbitrage (opposition cannot simply invoke the decree without legitimacy cost).
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE IMPERIAL REINTERPRETATION (PITON) — From the vantage of the Empire, the contested legality of the SCU becomes moot through replacement. Imperial authority transforms the decree into theatrical preservation of republican form — the Senate still 'can' vote it, but the emperor's power has rendered it functionally inert. Theater ratio high: the form persists but the mechanism no longer functions. The Empire arbitrages away from the SCU's actual use while maintaining its symbolic legality. Civilizational horizon reflects the centuries-long degradation of the mechanism.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the contested legality of emergency power appears as an immutable feature of any legal system: states face existential threats that cannot wait for due process, and legal systems must choose between preserving form or preserving the entity. This perspective naturalizes the SCU as an invariant constraint on republican law itself — a structural necessity, not a contested institutional choice. However, this perspective instantiates the oracle gap: the analytical observer's framework cannot detect what the 'legality contested' reading reveals — that the Republic's failure to resolve the contradiction (emergency without safeguards, law through law's suspension) was a choice available for revision, and that choice would have produced different outcomes.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senatus_consultum_ultimum__legality_contested_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senatus_consultum_ultimum__legality_contested_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senatus_consultum_ultimum__legality_contested_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senatus_consultum_ultimum__legality_contested_reading, TR),
    TR >= 0.70.

:- end_tests(senatus_consultum_ultimum__legality_contested_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Initial value (0.35) reflects that the decree begins as a coordination mechanism — the Senate genuinely needs emergency authority to respond to existential threats. Mid-interval value (0.48) marks Cicero's execution and trial, where the decree's legal status becomes actively contested — extractiveness rises as the mechanism reveals its capacity to impose retrospective legal judgment. Final value (0.58) reflects the entrenched pattern: the decree normalizes executive extrajudicial killing, successive invocations accumulate precedent for its use against political opponents, and the extractiveness ratchets upward as each faction learns it can use the decree against rivals. Suppression (0.68): Moderate-high. The decree operates precisely by eliminating due process (trial, appeal, legal defense) — this is suppression by design. Suppression increases over the interval as successful uses of the decree make alternatives less visible. Theater ratio (0.55): Moderate. The decree maintains significant performative content — the Senate must vote it, the consul must exhort it, legal arguments must be made. But the mechanism does real harm (death, exile), so theater is below 0.70. As the Empire replaces the decree, theater would rise (it persists symbolically while functionally inert).
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal why the legality was genuinely contested. The senatorial faction (rope perspective) sees the decree as legitimate emergency coordination — 'the state needed this power to survive.' The executed citizens (snare perspective) see it as lawless killing. The executing consul (tangled rope perspective) sees legitimate authorization that later becomes illegitimate when political enemies reinterpret it — the gap is temporal. The republican legal tradition (identity_locked tangled rope) sees the decree as forcing it to betray its own principles — the gap is between procedure and function. The reformist opposition (constrained tangled rope) sees it as normalization of factional violence — the gap is between justification and precedent. The analytical observer (mountain perspective) risks seeing this as inherent to all emergency law, when the reading reveals it as a specific institutional failure: the Republic never resolved whether the decree was constitutional or extra-legal, and that unresolved contradiction was what destabilized it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values differ sharply across perspectives due to structural position. Executed citizens have d ≈ 0.95 (full victims, trapped → f(d) ≈ 1.42, highest experienced extraction). Executing consuls have d ≈ 0.60 (mixed: beneficiary of senatorial backing, victim of later legal liability → f(d) ≈ 0.85, moderate extraction). Senatorial faction has d ≈ 0.05 (beneficiary with arbitrage, decoupled from legal liability → f(d) ≈ -0.12, negative extraction). Republican legal tradition has d ≈ 0.65 (victim of self-undermining, identity-locked → f(d) ≈ 1.00). The reformist opposition has d ≈ 0.55 (organized victim, constrained → f(d) ≈ 0.75). These differentials are the reading's core diagnostic signal: the decree extracts from different agents at different rates, and the political collapse of the Republic tracks the accumulation of this asymmetry.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_authority_in_emergency,
    'Can emergency power be legally authorized without the legal procedure that normally authorizes power? Does suspension of procedure require prior procedural authorization?',
    'Jurisprudential reconstruction: analysis of the Republic''s own legal arguments (Cicero''s defense, senatorial doctrine, tribunes'' challenge); comparison with other legal systems facing the same dilemma',
    'If yes (emergency requires prior authorization): the SCU is a legitimate constitutional mechanism — legal because authorized. If no (emergency is self-authorizing): the SCU is extra-legal by definition — its ''legality'' is a conceptual confusion masking what was actually a coup. This splits the reading into two substrates: ''contested legality'' becomes either ''genuinely ambiguous'' or ''false legitimacy claim.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_authority_in_emergency, conceptual, 'Whether emergency power can be legally authorized without legal procedure').

omega_variable(
    procedural_versus_substantive_legality,
    'Does the republic distinguish between procedural legality (was the decree voted correctly?) and substantive legality (is the outcome consistent with legal principles)? Do those standards align or conflict in the SCU cases?',
    'Close reading of Cicero''s trial speeches, senatorial debate records, and tribunes'' objections; mapping of which actors prioritized procedure vs. substance in defending or attacking the decree''s use',
    'If procedurally legal but substantively illegal (the decree was voted correctly but its use violated principles): the legitimacy crisis is fundamental — law is self-undermining. If both procedurally and substantively legal: the ''contested'' framing is rhetorical rather than structural. If neither: the decree was always extra-legal and the contest is about whether to admit it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_versus_substantive_legality, empirical, 'Alignment between procedural and substantive legality in SCU cases').

omega_variable(
    beneficiary_drift_over_time,
    'Does the decree''s beneficiary shift between senatorial factions over time, revealing that ''emergency necessity'' was actually factional advantage?',
    'Chronological analysis of SCU invocations: which factions benefited in each case, whether the same faction consistently invoked it, whether opposition factions would have invoked it differently if in power',
    'If beneficiary is stable (same faction always benefits): emergency power is factional extraction machinery. If beneficiary drifts (any faction in power invokes it): emergency power is genuinely neutral coordination. The drift pattern determines whether the extract-apparatus classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_drift_over_time, empirical, 'Whether SCU beneficiaries shift or remain constant across political cycles').

omega_variable(
    reading_contest_location,
    'Where exactly is the legality contested — in the senatorial vote authorizing the decree, in the consul''s interpretation of the exhortation, in the citizen''s right to trial, or in the legitimacy of ex post facto judgment by courts?',
    'Mapping of the legal arguments: Cicero claimed senatorial authorization + state necessity; prosecutors later claimed the execution violated citizens'' legal rights; both cannot be true in the same framework. Which claim is foundational?',
    'The location of contestation determines the reading''s relationship to sibling readings. If contestation is in the authorization, this reading forecloses emergency_without_office_reading (you either have legal authorization or you don''t). If contestation is in the use, this reading coexists_with gracchan_precedent_reading (the legality is contested and the precedents are weapons — both are true).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Precise location of legal contestation in the SCU mechanism').

omega_variable(
    exile_as_extraction_mechanism,
    'Is Cicero''s exile extraction imposed by the decree (the decree legitimates killing him later through exile), or extraction imposed on the decree (political enemies use retrospective illegality judgment to remove him)?',
    'Sequencing analysis: did the decree''s supporters ever argue exile was justified by the decree itself, or was exile always framed as a separate political decision? Did the decree''s language authorize or prohibit ex post facto judgment?',
    'If the decree extracts via delayed exile: the decree''s victim set expands to include executors, revealing true asymmetry (executors pay extraction costs later). If exile is external punishment: the decree''s extraction is immediate (dead citizens) and the executor''s vulnerability is a separate political fact. This affects whether the executor''s classification as tangled_rope or snare is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exile_as_extraction_mechanism, empirical, 'Whether exile functioned as extraction mechanism of the SCU itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senatus_consultum_ultimum__legality_contested_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scu_legality_theater_t0, senatus_consultum_ultimum__legality_contested_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(scu_legality_theater_t5, senatus_consultum_ultimum__legality_contested_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(scu_legality_theater_t10, senatus_consultum_ultimum__legality_contested_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(scu_legality_extr_t0, senatus_consultum_ultimum__legality_contested_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scu_legality_extr_t5, senatus_consultum_ultimum__legality_contested_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scu_legality_extr_t10, senatus_consultum_ultimum__legality_contested_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scu_legality_supp_t0, senatus_consultum_ultimum__legality_contested_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(scu_legality_supp_t5, senatus_consultum_ultimum__legality_contested_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(scu_legality_supp_t10, senatus_consultum_ultimum__legality_contested_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senatus_consultum_ultimum__legality_contested_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__legality_contested_reading, senatus_consultum_ultimum__emergency_without_office_reading).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__legality_contested_reading, senatus_consultum_ultimum__gracchan_precedent_reading).

% DUAL FORMULATION NOTE:
% The SCU kernel has three readings with distinct ε values and structural features. This reading (legality_contested: ε=0.58, tangled_rope) emphasizes the Republic's unresolved legal contest. The emergency_without_office_reading (ε≈0.70, snare candidate) emphasizes lack of structural safeguards. The gracchan_precedent_reading (ε≈0.75, snare) emphasizes use as factional weapon from inception. All three readings describe the same historical mechanism but with different ε values because they measure different observable features — legality in the courts, safeguard structure, factional pattern. The readings are linked because they compete to explain the Republic's failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(senatus_consultum_ultimum__legality_contested_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
