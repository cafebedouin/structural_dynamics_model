% ============================================================================
% CONSTRAINT STORY: active_passive_citizenship__exclusion_of_the_poor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_active_passive_citizenship__exclusion_of_the_poor_reading, []).

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
 *   constraint_id: active_passive_citizenship__exclusion_of_the_poor_reading
 *   human_readable: Active/Passive Citizenship Split: Exclusion of the Poor (1791 French Revolution)
 *   domain: legal/political/doctrinal
 *
 * SUMMARY:
 *   The active/passive citizenship split enacted in the 1791 French
 *   Constitution betrayed the Declaration of the Rights of Man and Citizen
 *   proclaimed just two years earlier. The Declaration's Article 1 asserted
 *   the universality of human rights ('Men are born and remain free and equal
 *   in rights'), but the Constitution restricted political participation to
 *   'active citizens' — those who paid a tax quota set high enough to exclude
 *   the vast majority of the population. This constraint embodies a central
 *   revolutionary contradiction: the split simultaneously enacted
 *   coordination (a mechanism to select voters) and extraction (the
 *   systematic exclusion of the poor from political voice). The constraint is
 *   best analyzed as a READING — one specific interpretation of a contested
 *   kernel. The kernel is the meaning of citizenship and the relationship
 *   between universal human rights (proclaimed) and unequal citizen rights
 *   (implemented). This reading emphasizes the betrayal: the split suppressed
 *   the universalism just proclaimed and concentrated political voice in the
 *   hands of the censitary electorate (property owners). The readings
 *   coexist: another observer would emphasize the property_franchise_logic
 *   reading (the mechanism as coherent theory of independence), and yet
 *   another would foreground the gender exclusion. All three readings are
 *   live, holding simultaneously across different analytical positions.
 *
 * KEY AGENTS:
 *   - Passive Citizens / The Poor: Primary victims (powerless/trapped) — declared equal in humanity, legally stripped of political agency; taxation without representation; no material path to active citizenship
 *   - Censitary Electorate / Propertied Classes: Primary beneficiaries (institutional/arbitrage) — exclusive access to political power, justified by theory of independence; can shift status via property arbitrage
 *   - Constitutional Framers (Sieyès, Thouret, etc.): Institutional architects (institutional/arbitrage) — defined the threshold, managed the contradiction between Declaration and Constitution, treated the split as coherent principle rather than betrayal
 *   - Democratic Opposition / Jacobins: Organized challengers (organized/constrained) — perceived the split as temporary, demanded universal male suffrage as the logical endpoint of the Declaration
 *   - Women (entire category): Secondary victims (powerless/trapped) — excluded entirely from both active and passive citizenship; existed outside the split mechanism entirely; de Gouges articulated the absence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent censitary principle as inherent to any legitimate franchise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(active_passive_citizenship__exclusion_of_the_poor_reading, 0.62).
domain_priors:suppression_score(active_passive_citizenship__exclusion_of_the_poor_reading, 0.78).
domain_priors:theater_ratio(active_passive_citizenship__exclusion_of_the_poor_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(active_passive_citizenship__exclusion_of_the_poor_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(active_passive_citizenship__exclusion_of_the_poor_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(active_passive_citizenship__exclusion_of_the_poor_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(active_passive_citizenship__exclusion_of_the_poor_reading, tangled_rope).
narrative_ontology:human_readable(active_passive_citizenship__exclusion_of_the_poor_reading, "Active/Passive Citizenship Split: Exclusion of the Poor (1791 French Revolution)").
narrative_ontology:topic_domain(active_passive_citizenship__exclusion_of_the_poor_reading, "legal/political/doctrinal").

domain_priors:requires_active_enforcement(active_passive_citizenship__exclusion_of_the_poor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(active_passive_citizenship__exclusion_of_the_poor_reading, 'bedd243a-e2a1-4bdb-8a63-12c772337847').
narrative_ontology:cs_kernel_codification('bedd243a-e2a1-4bdb-8a63-12c772337847', formalized).
narrative_ontology:cs_authority_grounding('bedd243a-e2a1-4bdb-8a63-12c772337847', extraction).
narrative_ontology:cs_interpretation_layer_present('bedd243a-e2a1-4bdb-8a63-12c772337847').
narrative_ontology:cs_reading_relation('bedd243a-e2a1-4bdb-8a63-12c772337847', active_passive_citizenship__property_franchise_logic_reading, coexists_with).
narrative_ontology:cs_reading_relation('bedd243a-e2a1-4bdb-8a63-12c772337847', active_passive_citizenship__women_excluded_reading, influences).
narrative_ontology:cs_axiom('bedd243a-e2a1-4bdb-8a63-12c772337847', foundational, universal_declaration_betrayed).
narrative_ontology:cs_axiom_status(universal_declaration_betrayed, holdable).
narrative_ontology:cs_axiom_grounding('bedd243a-e2a1-4bdb-8a63-12c772337847', universal_declaration_betrayed, deontological).
narrative_ontology:cs_axiom('bedd243a-e2a1-4bdb-8a63-12c772337847', foundational, suppression_of_poor_is_illegitimate).
narrative_ontology:cs_axiom_status(suppression_of_poor_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('bedd243a-e2a1-4bdb-8a63-12c772337847', suppression_of_poor_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('bedd243a-e2a1-4bdb-8a63-12c772337847', universal_human_rights_framework).
narrative_ontology:cs_drift_state('bedd243a-e2a1-4bdb-8a63-12c772337847', constitutional_implementation_1791_1792, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bedd243a-e2a1-4bdb-8a63-12c772337847', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(active_passive_citizenship__exclusion_of_the_poor_reading, active_passive_citizenship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(active_passive_citizenship__exclusion_of_the_poor_reading, censitary_electorate).
narrative_ontology:constraint_beneficiary(active_passive_citizenship__exclusion_of_the_poor_reading, propertied_classes).
narrative_ontology:constraint_victim(active_passive_citizenship__exclusion_of_the_poor_reading, passive_citizens_poor).
narrative_ontology:constraint_victim(active_passive_citizenship__exclusion_of_the_poor_reading, political_voice_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE CITIZEN (SNARE) — Declared equal in rights of man, legally stripped of political rights as passive citizen. Bears the full extraction: taxation without representation, obligation without voice, membership without agency. No exit option — legal status is imposed. Maximum experienced extraction of political voice. The betrayal is complete at biographical horizon: one lifetime under the constraint, no path to active citizenship except through property acquisition inaccessible to the poor.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BORDERLINE AGENT (TANGLED ROPE) — Agents near the property threshold experience mixed coordination and extraction. The property qualification both coordinates (it does select a class with stable interests and some leisure for deliberation) and extracts (the threshold is set to exclude the vast majority, and the borderline agent may be one harvest or bad year away from passive status). Exit is constrained: property acquisition is possible but difficult, and loss of status is catastrophic. Moderate experienced extraction — some agency, some vulnerability, some benefit from the stability the mechanism provides.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACTIVE CITIZEN (ROPE) — Beneficiary of the split. Experiences the constraint as coordination: the property requirement filters out the 'dependent' and concentrates political power among those with independent judgment (as the reading claims). Sees the mechanism as solving the coordination problem of 'who can vote freely without pressure from employers or patrons.' Has full exit via property arbitrage if threatened — can move wealth, change status, or exit the electorate voluntarily. Net beneficiary — extraction flows toward this agent. Rope classification: genuine coordination function exists alongside concentration of benefit.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC OPPOSITION (SCAFFOLD) — Organized opposition to the censitary restriction (Jacobins, democratic clubs, sans-culottes) sees the split as a temporary arrangement destined for sunset. They claim universal male suffrage as the logically inevitable endpoint of the Declaration's universalism. Low effective extraction from their perspective because they perceive an exit path: democratic revolution will dissolve the property qualification and extend active citizenship to all. The scaffold is unstable — the 1793 universal male suffrage is a brief moment before Thermidor narrows again. But the organized agents' perception of the mechanism as temporary is structurally sound.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL THEATER (PITON) — The written Constitution treats the active/passive split as a formal legal category with defined criteria, measured in tax contribution. The mechanism is largely performative: it claims to identify the 'independent' through property, but this is cover story for concentrated power. The Constitution sees itself as solving an abstract problem (who is fit to deliberate?) via a measurable criterion (tax payment). But the real function — excluding the poor from political voice — is not discussed in the document itself. The theater ratio is moderate (0.55) because the mechanism does perform some genuine work (it does concentrate voters among the propertied, it does require administration, it does have measurable criteria), but the primary function is political exclusion, not epistemic filtering. The constitutional language persists through institutional inertia even as democratic theory shifts.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the censitary principle appears as an immutable natural law: 'only those with independent material interest can vote independently; poverty implies dependence; therefore property qualification is inherent to any legitimate franchise system.' This perspective naturalizes the mechanism as following from economic logic or human nature. However, this is a false summit: the structural data shows clear beneficiaries (the propertied), clear victims (the poor), clear enforcement mechanisms, and clear contingency (the split contradicts the Declaration's own universalism). The 'natural law' framing is itself the constraint's cover story — naturalizing contingent institutional power as inevitable economic law.
constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(active_passive_citizenship__exclusion_of_the_poor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(active_passive_citizenship__exclusion_of_the_poor_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(active_passive_citizenship__exclusion_of_the_poor_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(active_passive_citizenship__exclusion_of_the_poor_reading, TR),
    TR >= 0.70.

:- end_tests(active_passive_citizenship__exclusion_of_the_poor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint extracts political voice from the poor — they are taxed but have no vote, no deliberative role, and no formal path to political agency. The extractiveness is not maximal (0.72+) because the constraint also performs genuine coordination (it does concentrate voters, it does require administration, it does have measurable criteria), and because the poor were not suddenly stripped of rights they previously held — they are new members of a state with a new politics. The measurement trajectory (0.58 → 0.62 → 0.65) reflects the hardening of the mechanism as the Constitution's implementation proceeds and as the contradiction between Declaration and Constitution becomes more stark. Suppression (0.78): High. The mechanisms that suppress alternatives to censitary restriction are formidable: legal prohibition (passive citizens cannot vote by law), material barriers (property acquisition is economically inaccessible to most), and ideological cover (the independence theory frames the restriction as necessary and natural). The suppression rises slightly over time (0.72 → 0.78 → 0.81) as the enforcement apparatus solidifies and the justifying ideology thickens. Theater ratio (0.55): Moderate. The mechanism includes genuine coordination work (defining the tax threshold, maintaining voter rolls, organizing electoral assemblies) but also performative work (the language of independence, the implicit suggestion that poor people 'lack independence' rather than lack capital). The theater rises (0.48 → 0.55 → 0.58) as constitutional doctrine elaborates the justification for the split.
 *
 * PERSPECTIVAL GAP:
 *   This reading demonstrates sharp perspectival divergence. The passive citizen (powerless/trapped) experiences pure extraction (Snare) — promised equality, legally denied voice, no path to active status. The censitary elector (institutional/arbitrage) experiences coordination (Rope) — the mechanism genuinely selects for deliberative capacity (as they understand it) and protects them from manipulation. The borderline agent (moderate/constrained) experiences mixed effects (Tangled Rope) — the mechanism both coordinates and extracts, and their status is precarious. The organized opposition (organized/constrained) sees a temporary problem (Scaffold) — they believe universal suffrage is the inevitable endpoint. The constitutional doctrine (institutional/arbitrage) treats the split as coherent legal principle (Piton of institutional inertia, because the mechanism is already becoming performance of principle rather than pure coordination). The analytical observer (analytical/analytical) risks naturalizing the split as inevitable economic law (Mountain), but the structural evidence reveals this as a false summit: the property threshold is politically contingent, not naturally determined.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the constraint. Passive citizens: victims + trapped exit → d ≈ 0.92 → f(d) ≈ 1.35 → high experienced extractiveness. Censitary electors: beneficiaries + arbitrage exit → d ≈ 0.08 → f(d) ≈ -0.14 → low/negative experienced extractiveness. Borderline agent: mixed position + constrained exit → d ≈ 0.48 → f(d) ≈ 0.60 → moderate experienced extractiveness. Organized opposition: agents seeking to dissolve the constraint + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate-high experienced extractiveness (they experience the constraint as oppressive even though they are organized). The analytical observer: observing the structure + analytical exit → d ≈ 0.73 → f(d) ≈ 1.15 → high scholarly extractiveness (the observer must account for all perspectives simultaneously and sees the mechanism's hidden structure). No directionality overrides are needed — the derivation chain captures the actual structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the split is both genuinely extractive AND genuinely coordinating. It is not a false dilemma between 'extraction' and 'coordination' — it is a tangled rope: the mechanism coordinates the electorate (selects for stability, concentrates power among those with leisure to deliberate) while simultaneously extracting political voice from the poor (withholds citizenship rights from those who do not meet the property threshold). The mandatrophy is resolved by showing that both are true. The extraction is visible from the powerless perspective (Snare), the coordination is visible from the beneficiary perspective (Rope), and the analytical observer must hold both simultaneously (Tangled Rope). The false summit (the analytical observer's natural law reading) is a diagnostic artifact: if we naturally assume that property-based franchise qualification is inevitable economic law, we have fallen into the constraint's own cover story. The structural data reveals the contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_threshold_contingency,
    'Is the property threshold that defines ''independence'' a natural reflection of economic reality, or a politically contingent choice to exclude the poor?',
    'Comparative historical analysis: (1) Do different historical periods set the threshold at materially different levels without changing who is actually independent? (2) Do contemporary systems that claim to measure independence via property use vastly different thresholds for the same nominal criterion? (3) Do systems that use alternative independence criteria (literacy, ownership of tools, membership in guilds) produce materially different electorates?',
    'If contingent: the ''independence'' framing is post-hoc rationalization, and the constraint is pure extraction via exclusion (Snare tendencies for the powerless). If natural: the constraint might be a genuine coordination mechanism with unintended exclusionary effects (stronger Rope/Tangled Rope reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independence_threshold_contingency, empirical, 'Whether property-based independence criterion is natural or contingent').

omega_variable(
    declaration_repudiation_authenticity,
    'Did the Constitution''s drafters consciously repudiate the Declaration''s universalism, or did they believe the active/passive split was compatible with ''equal rights of man''?',
    'Textual analysis of constitutional debates (Sieyès speeches, Thouret committee records, Barnave interventions); comparison with contemporary egalitarian critiques (Anacharsis Cloots, Jean-Paul Marat); assessment of whether defenders of censitary restriction invoked the Declaration at all or simply ignored the contradiction.',
    'If conscious repudiation: the split represents explicit betrayal, which strengthens the snare/extraction reading. If unconscious: the constraint may be theoretically incoherent but not deliberately malicious, complicating the moral reading while not changing the structural mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_repudiation_authenticity, empirical, 'Whether constitutional framers consciously repudiated Declaration universalism').

omega_variable(
    passive_citizenship_acquiescence_scope,
    'To what extent did passive citizens themselves accept the status as temporary or legitimate, versus viewing it as illegitimate from the moment of proclamation?',
    'Historical study of passive citizen discourse: Did they petition for voting rights, or did they accept the theoretical case that they lacked independence? Did the legitimacy of the split rest on passive citizens'' acceptance, or on state enforcement regardless of acceptance?',
    'If widespread acquiescence: suppression may be lower than structural analysis suggests (internalized frame, not coerced silence). If widespread rejection: suppression is higher and more coercive than the mechanism''s formal language indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_citizenship_acquiescence_scope, empirical, 'Scope of passive citizen acceptance or rejection of the status').

omega_variable(
    reading_contra_property_franchise_logic,
    'Is this reading (the split as betrayal of universalism) logically compatible with the property_franchise_logic_reading (the split as coherent theory of independence), or do they foreclose each other?',
    'Logical analysis: Can both readings hold simultaneously in a single framework? The property_franchise_logic reading claims the threshold measures genuine independence. This reading claims the split betrays universalism by suppressing the poor. A third party could affirm both: ''Yes, the mechanism is theoretically coherent, AND yes, it betrays the Declaration''s promise of universal rights.'' The readings coexist — one explains the mechanism''s internal logic, the other critiques the gap between promise and delivery.',
    'If compatible (coexists_with): both readings remain live across different observers. If incompatible (forecloses): accepting one''s core premise denies the other''s. The structural evidence suggests coexistence — the readings are answering different questions (How does the mechanism work? vs. Is the mechanism just?)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contra_property_franchise_logic, conceptual, 'Logical compatibility of poor_exclusion and property_franchise_logic readings').

omega_variable(
    gender_intersection_with_censitary_split,
    'Does this reading (exclusion of the poor) foreclose, coexist with, or influence the women_excluded_reading?',
    'Structural analysis: The women_excluded_reading centers on exclusion of women entirely from active citizenship (and mostly from passive citizenship). This reading centers on property-based exclusion of the poor. A poor woman is excluded twice — once for poverty, once for gender. A wealthy woman is excluded once — for gender. Do these exclusions operate in the same mechanism or in separate mechanisms? Can a framework affirm both readings simultaneously without contradiction?',
    'If coexists_with: both exclusions are live and simultaneous, and acknowledging one does not require denying the other. If influences: the censitary split creates conditions that make gender exclusion easier to justify or enforce. If forecloses: adopting this reading''s framework would logically deny the necessity of gender exclusion as a separate category.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_intersection_with_censitary_split, conceptual, 'Relationship between poor-exclusion and gender-exclusion mechanisms').

omega_variable(
    beneficiary_consciousness_contradiction,
    'Did the beneficiaries (the censitary electorate and propertied classes) perceive themselves as benefiting from an exclusionary mechanism, or did they genuinely believe they were implementing a principle of independence?',
    'Historical study of propertied class rhetoric: Did they defend the exclusion as necessary for poor people''s own good (paternalism), or as necessary to protect propertied interests, or as a principled test of independence? The answer determines whether the mechanism was experienced as extraction (with cover story) or as coordination with side effects.',
    'If genuine belief in the principle: the constraint may be experienced by beneficiaries as rope rather than tangled_rope. If conscious extraction: it strengthens the tangled_rope reading with clear understanding that this is asymmetric power. If mixed (some genuine belief, some cynical extraction): the piton perspective becomes more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_consciousness_contradiction, empirical, 'Whether beneficiaries perceived mechanism as principled or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(active_passive_citizenship__exclusion_of_the_poor_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(appcer_theater_t0_raw_split, active_passive_citizenship__exclusion_of_the_poor_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(appcer_theater_t1_constitutional_language, active_passive_citizenship__exclusion_of_the_poor_reading, theater_ratio, 1, 0.55).
narrative_ontology:measurement(appcer_theater_t2_ideology_thickens, active_passive_citizenship__exclusion_of_the_poor_reading, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(appcer_extract_t0_proclamation, active_passive_citizenship__exclusion_of_the_poor_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(appcer_extract_t1_constitution_year, active_passive_citizenship__exclusion_of_the_poor_reading, base_extractiveness, 1, 0.62).
narrative_ontology:measurement(appcer_extract_t2_enforcement, active_passive_citizenship__exclusion_of_the_poor_reading, base_extractiveness, 2, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(appcer_supp_t0_proclamation, active_passive_citizenship__exclusion_of_the_poor_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(appcer_supp_t1_constitution, active_passive_citizenship__exclusion_of_the_poor_reading, suppression_requirement, 1, 0.78).
narrative_ontology:measurement(appcer_supp_t2_enforcement_hardened, active_passive_citizenship__exclusion_of_the_poor_reading, suppression_requirement, 2, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(active_passive_citizenship__exclusion_of_the_poor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(active_passive_citizenship__exclusion_of_the_poor_reading, 0.18).
narrative_ontology:affects_constraint(active_passive_citizenship__exclusion_of_the_poor_reading, active_passive_citizenship__property_franchise_logic_reading).
narrative_ontology:affects_constraint(active_passive_citizenship__exclusion_of_the_poor_reading, active_passive_citizenship__women_excluded_reading).
narrative_ontology:affects_constraint(active_passive_citizenship__exclusion_of_the_poor_reading, french_revolutionary_temporality__universal_claims).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the active_passive_citizenship kernel. The network links to sibling readings (property_franchise_logic_reading, women_excluded_reading) which instantiate alternative interpretations of the same constitutional split. Each reading has its own ε, its own beneficiary/victim structure, and its own perspectival classification profile. The readings are not ordered hierarchically — they coexist as competing analytical framings of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
