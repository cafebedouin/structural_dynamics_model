% ============================================================================
% CONSTRAINT STORY: terror_coincidence__legitimation_during_purge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terror_coincidence__legitimation_during_purge_reading, []).

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
 *   constraint_id: terror_coincidence__legitimation_during_purge_reading
 *   human_readable: Constitution as Terror's Alibi: Legitimation Reading (1936 Soviet Purge)
 *   domain: legal/doctrinal/political_terror
 *
 * SUMMARY:
 *   This constraint captures a specific historical reading of the 1936 Soviet
 *   constitution during the Great Purge: the text functioned as an alibi for
 *   mass terror. The constitution proclaimed 'the most democratic in the
 *   world,' guaranteeing equal rights, due process, and protection from
 *   arbitrary arrest — while the NKVD (secret police) operated under explicit
 *   quota orders to arrest and execute specific numbers of 'enemies,'
 *   prosecute show trials in which verdicts were predetermined, and liquidate
 *   Old Bolsheviks, party cadres, military officers, and entire ethnic
 *   populations by numerical decree. This reading construes the constitution
 *   not as a sincere blueprint for governance (sincere_blueprint_reading) or
 *   as plebiscitary theater (plebiscitary_theater_reading), but as an active
 *   instrument of legitimation: the text enabled the regime to claim
 *   legality, masked suppression under procedure, and provided international
 *   propaganda value ('Look, we have a constitution more progressive than the
 *   West'). The constraint is operative across multiple timeframes: immediate
 *   (the troika victim is sentenced under constitutional procedures);
 *   biographical (lawyers and judges are coerced into participation);
 *   civilizational (international legal scholars cite the constitution as
 *   evidence of socialist progress, unwittingly legitimizing a document that
 *   conceals mass murder).
 *
 * KEY AGENTS:
 *   - Quota-sentenced prisoners (troika victims): Powerless/trapped — bears maximal extraction. Receives rights on paper; executed by decree in practice.
 *   - Secret police apparatus (NKVD leadership): Institutional/arbitrage — benefits from the constitution's legitimacy cover; gains deniability for mass execution.
 *   - Regime self-presentation (diplomacy, international propaganda): Institutional/arbitrage — uses the constitution's text to claim legitimacy globally while terror is hidden from international view.
 *   - Mid-level functionaries (quota administrators, regional cadres): Powerful/mobile — execute quotas using constitutional language; experience both benefit (security function) and extraction (moral complicity, coercion).
 *   - Legal professionals (defense lawyers, judges outside the apparatus): Organized/constrained — bound by constitutional oath; coerced into legitimizing predetermined outcomes.
 *   - International legal scholars: Powerful/constrained — harvested as intellectual witnesses to constitutional legitimacy; later discovered to have amplified an alibi for mass murder.
 *   - The regime itself (Stalin's faction): Institutional/arbitrage — ultimate beneficiary; deliberately uses the constitution as cover.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terror_coincidence__legitimation_during_purge_reading, 0.88).
domain_priors:suppression_score(terror_coincidence__legitimation_during_purge_reading, 0.92).
domain_priors:theater_ratio(terror_coincidence__legitimation_during_purge_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terror_coincidence__legitimation_during_purge_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(terror_coincidence__legitimation_during_purge_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(terror_coincidence__legitimation_during_purge_reading, theater_ratio, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terror_coincidence__legitimation_during_purge_reading, snare).
narrative_ontology:human_readable(terror_coincidence__legitimation_during_purge_reading, "Constitution as Terror's Alibi: Legitimation Reading (1936 Soviet Purge)").
narrative_ontology:topic_domain(terror_coincidence__legitimation_during_purge_reading, "legal/doctrinal/political_terror").

domain_priors:requires_active_enforcement(terror_coincidence__legitimation_during_purge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(terror_coincidence__legitimation_during_purge_reading, 'b174a7f3-cb14-411b-9014-a219193f6955').
narrative_ontology:cs_kernel_codification('b174a7f3-cb14-411b-9014-a219193f6955', formalized).
narrative_ontology:cs_authority_grounding('b174a7f3-cb14-411b-9014-a219193f6955', extraction).
narrative_ontology:cs_interpretation_layer_present('b174a7f3-cb14-411b-9014-a219193f6955').
narrative_ontology:cs_reading_relation('b174a7f3-cb14-411b-9014-a219193f6955', terror_coincidence__plebiscitary_theater_reading, coexists_with).
narrative_ontology:cs_reading_relation('b174a7f3-cb14-411b-9014-a219193f6955', terror_coincidence__sincere_blueprint_reading, forecloses).
narrative_ontology:cs_axiom('b174a7f3-cb14-411b-9014-a219193f6955', foundational, constitution_weaponized_for_alibi).
narrative_ontology:cs_axiom_status(constitution_weaponized_for_alibi, holdable).
narrative_ontology:cs_axiom_grounding('b174a7f3-cb14-411b-9014-a219193f6955', constitution_weaponized_for_alibi, empirically_contingent).
narrative_ontology:cs_axiom('b174a7f3-cb14-411b-9014-a219193f6955', secondary, legal_procedures_substitute_for_substantive_justice).
narrative_ontology:cs_axiom_status(legal_procedures_substitute_for_substantive_justice, holdable).
narrative_ontology:cs_axiom_grounding('b174a7f3-cb14-411b-9014-a219193f6955', legal_procedures_substitute_for_substantive_justice, empirically_contingent).
narrative_ontology:cs_reference_frame('b174a7f3-cb14-411b-9014-a219193f6955', rule_of_law_constitutional_governance).
narrative_ontology:cs_drift_state('b174a7f3-cb14-411b-9014-a219193f6955', great_purge_enforcement, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('b174a7f3-cb14-411b-9014-a219193f6955', '').
narrative_ontology:cs_kernel_id(terror_coincidence__legitimation_during_purge_reading, terror_coincidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(terror_coincidence__legitimation_during_purge_reading, regime_self_presentation).
narrative_ontology:constraint_beneficiary(terror_coincidence__legitimation_during_purge_reading, secret_police_apparatus).
narrative_ontology:constraint_beneficiary(terror_coincidence__legitimation_during_purge_reading, quota_enforcement_machinery).
narrative_ontology:constraint_victim(terror_coincidence__legitimation_during_purge_reading, quota_sentenced_prisoners).
narrative_ontology:constraint_victim(terror_coincidence__legitimation_during_purge_reading, legal_due_process).
narrative_ontology:constraint_victim(terror_coincidence__legitimation_during_purge_reading, constitutional_legitimacy_concept).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: QUOTA-SENTENCED PRISONER (SNARE) — Sentenced to death or camp by numerical decree irrespective of evidence or conduct. The new constitution has just proclaimed their equality and rights; within hours or days, the troika executes them by quota. Zero exit options. Maximum suppression — the legal text itself becomes the instrument of deception that masks the execution order. The prisoner experiences pure extraction: their life is extracted under the alibi of democratic procedure.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGAL PROFESSIONALS NOT IN THE TERROR APPARATUS (SNARE) — Bound by oath to the new constitution's guarantees of defense and due process. Cannot openly refuse (career destruction, arrest); cannot openly comply (would be complicit in show trials). High suppression, constrained exit. The constitution's formal text coerces participation in its own subversion — lawyers must use the written law to defend against accusations that the law is supposed to shield. Extraction of moral agency under the color of legal legitimacy.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-LEVEL FUNCTIONARIES / QUOTA ADMINISTRATORS (TANGLED ROPE) — Receive quota orders from above; must report arrests/executions meeting the numbers. The constitution provides the performative legitimacy they need to justify the orders to themselves and to civilian populations. They genuinely coordinate state security (a real function) while executing numerical extraction. Some have modest exit capacity (can be reassigned, can refuse and face demotion rather than death); they experience both benefit (security apparatus function) and extraction (complicity, coercion). The constitution launders suppression into procedure.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL SECRET POLICE LEADERSHIP (ROPE) — Sees the constitution as enabling efficient terror: a framework that justifies mass arrest under the language of socialist legality. They have high exit capacity (can redirect the apparatus, can be removed by Stalin but not by civilians). The constitution coordinates the terror machinery while providing deniability — the purge proceeds under 'the law,' not under naked dictatorship. Net beneficiary of the legitimacy structure. Low experienced extraction because they set the terms.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGIME INTERNATIONAL PRESENTATION (ROPE) — Uses the constitution as alibi in diplomatic discourse: 'Look, we have a constitution. We are a state of law.' The text enables the regime to claim legitimacy while conducting terror. They benefit from the contrast between the written law and the hidden practices — the contrast itself is valuable propaganda. High exit capacity at the institutional level (can adjust narratives, can emphasize or de-emphasize constitutional claims). Coordination function: making terror compatible with a (false) claim to constitutional governance.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SCHOLARS (SNARE) — The Soviet constitution is circulated as a model to progressive legal scholars globally. Many read the text genuinely — it is more elaborate than Western constitutions of the era — and see it as evidence of socialist legality advancing. By the time the terror is undeniable, the text has been cited as a precedent, embedded in comparative law, and used to argue that constitutional forms can exceed capitalist democracies. The scholars' extraction: their intellectual authority is harvested to legitimize a document that actively conceals mass murder. They cannot recover the text's credibility once the terror becomes known. The constitution's sophistication makes it a more effective alibi than a cruder document would have been.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LEGAL FORMALISM (MOUNTAIN) — From a civilizational perspective grounded in legal formalism, some constraint seems immutable: the gap between law-as-text and law-as-enforced is a structural feature of any legal system. No constitution prevents its own misuse. This perspective risks seeing the 1936 constitution's subversion as an inevitable legal phenomenon rather than a contingent choice. However, the structural data contradicts this mountain reading — the extraction is not inherent to constitutional form but to this regime's deliberate decision to use the text as alibi. The engine will flag this as a false summit, revealing that 'law is always subject to misuse' naturalizes what is actually a specifc extractive strategy.
constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terror_coincidence__legitimation_during_purge_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terror_coincidence__legitimation_during_purge_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(terror_coincidence__legitimation_during_purge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(terror_coincidence__legitimation_during_purge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The constraint extracts life (mass execution by quota), freedom (incarceration under predetermined sentencing), and moral agency (coercing legal professionals to participate in their own perversion). The extraction is not incidental to the constraint; it is the constraint's purpose. The constitutional text is the mechanism by which extraction is laundered into procedure. The rising trajectory (0.72 → 0.88 → 0.92) reflects the intensification of the terror: as the purge escalates, reliance on the constitution's legitimacy increases because the numbers grow and the need for cover becomes more acute. Suppression (0.92): Extreme. Multifaceted: (1) No legitimate exit for victims — arrest is extrajudicial in form (quota-based) and judicial in appearance (trial-based). (2) Suppression of alternatives to confession — torture is standard; denial of charges is impossible when the verdict is predetermined. (3) Suppression of institutional alternatives — independent judiciary is eliminated; all courts are subordinate to quotas. (4) Suppression of narrative alternatives — the regime monopolizes interpretation of the constitution; alternative readings are treason. The rising suppression (0.78 → 0.88 → 0.92) reflects hardening of enforcement mechanisms and tightening of procedural control. Theater ratio (0.95): Extreme. Show trials are pure performance — verdicts predetermined, confessions coerced, international observers invited to witness legality. The constitutional procedures are nearly all theater; the actual decisions (who is arrested, sentenced, executed) are made by quota, not by evidence or law. Lawyers perform defense; judges perform deliberation; courts perform justice. The rising trajectory (0.88 → 0.92 → 0.95) reflects the increasing sophistication of the theatrical apparatus as the purge becomes more visible and the need for legitimacy cover becomes more urgent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence. The quota-sentenced prisoner sees pure snare: rights promised and immediately violated. The mid-level functionary sees tangled rope: genuine security function mixed with extraction by quota. The secret police leadership sees rope: coordination mechanism providing legitimacy cover. The international scholar initially sees rope (a progressive constitution) but retrospectively sees snare (being weaponized as an alibi). The analytical formalist risks seeing mountain: 'any constitution can be misused; gap between law-as-text and law-as-enforced is inherent.' But the structural data contradicts the mountain reading — the extraction is contingent to this regime's choice, not inherent to constitutional form. The false summit detector will flag this. The deepest gap: between the sincere_blueprint_reading (some drafters meant it; Bukharin's hand is in the text) and this legitimation_during_purge_reading (the text was deliberately weaponized as an alibi). The contest between these readings is irreducible — both are live historical interpretations of whether the same document was sincerely drafted and then perverted, or deliberately crafted as cover for predetermined terror.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position relative to extraction. The quota-sentenced prisoner is a pure target (d ≈ 1.0) — the constraint extracts their life and freedom with no benefit. The secret police leadership is a pure beneficiary (d ≈ 0.0) — they gain deniability, legitimacy cover, and operational efficiency with no extraction cost. The mid-level functionary is mixed (d ≈ 0.65) — they execute quotas (target position) but also secure the state (beneficiary function). The international scholar is inverted (d ≈ 0.85) — they appear as beneficiaries of the constitution's prestige but are actually victimized by their own citation of a document weaponized for murder. The constitutional legitimacy concept itself has no agent position (it is a victim category, not an actor) — we treat it as a victim constraint to capture the long-term erosion of constitutionalism as a restraint on power that this constraint produces. The engine will derive chi from these d values via the sigmoid f(d), producing maximum chi for the powerless victim and minimum chi for the institutional beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. This constraint requires mandatrophy resolution because extractiveness > 0.70. The mandatrophy (the paradox that pure extraction cannot be sustained because extraction requires coordination, but maximum coordination cannot coexist with pure extraction) is resolved by the theatrical structure: the regime coordinates terror using the constitution's procedures as the coordination mechanism. The constitution provides the form (trials, verdicts, legal procedure) that enables mass killing to be organized, hidden, and justified. Mandatrophy is avoided by inverting the relationship between law and extraction: instead of law restraining extraction, law becomes the instrument of extraction. The theatre ratio (0.95) reflects this inversion — the system's coordination is almost entirely performative, but the performance IS the coordination mechanism. Show trials coordinate mass arrest by providing legal cover and victim selection procedures. The constraint sustains high extraction precisely because it substitutes procedural legitimacy for substantive justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_alibi_vs_naive_promise,
    'Did the regime deliberately draft the 1936 constitution as an alibi for the terror, or did the constitution''s democratic promises precede the decision to conduct the purge?',
    'Chronological analysis of drafting timeline vs terror escalation; internal Politburo records and directives; textual analysis of drafting iterations (Bukharin vs Stalin factions)',
    'If deliberately crafted: extraction is the primary function; alibi is intentional. Constitution''s legitimacy is a weapon. If drafted first with terror imposed later: the extraction is the constraint''s perversion, not its purpose — still a snare, but one that hijacked a sincere document. Changes the reading''s relationship to the sincere_blueprint_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_alibi_vs_naive_promise, empirical, 'Whether the constitution was deliberately designed as terror alibi').

omega_variable(
    scope_of_actual_enforcement_gap,
    'How wide was the gap between the constitution''s written protections and actual terror? Were show trials more theater than execution, or was quota sentencing the actual mechanism?',
    'Statistical analysis of trials vs. extrajudicial executions; composition of NKVD quotas (trial-convicted vs. arrested-and-shot); records of troika decisions vs. court verdicts',
    'If quotas were separate from trials (executive extraction): constitution served no role in actual sentencing — the alibi is pure theater. If quotas-by-trial was the mechanism: the constitution''s procedures were the actual extraction machinery. Affects whether this reading''s snare classification is accurate or overstates the constitution''s causal role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_actual_enforcement_gap, empirical, 'Ratio of judicial vs extrajudicial executions; role of formal trial in quota fulfillment').

omega_variable(
    reading_contest_foreclosure_structure,
    'Can the legitimation_during_purge_reading and the sincere_blueprint_reading coexist as live interpretations of the same document, or does accepting one preclude the other?',
    'Examine whether Bukharin''s influence (sincere reading) and Stalin''s terror apparatus (legitimation reading) operated on the same text or whether the text was substantively revised between drafting and enforcement. If revised: readings are sequential (sincere first, then perverted). If unchanged: readings are genuinely contesting the same document — both are live.',
    'If foreclosed: the legitimation reading rules out sincerity. If coexisting: both readings are held by different historical factions, and the contest is real and irreducible. Affects the reading_relations classification in cs_structure: forecloses vs coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_structure, conceptual, 'Whether sincere and legitimation readings of the 1936 constitution logically coexist or foreclose each other').

omega_variable(
    constitutional_legitimacy_victim_extraction,
    'Is ''constitutional legitimacy concept'' genuinely a victim of this constraint, or is extractiveness directed only at the human victims?',
    'Examine the long-term effect on global constitutional discourse: did the exposure of the 1936 constitution as alibi weaken formal constitutionalism as a governance form? Did it strengthen skepticism about law-as-text vs law-as-enforced? Did subsequent authoritarian regimes cite the 1936 example to argue constitutionalism is inherently performative?',
    'If yes: constitutional legitimacy is a victim; the constraint extracts not just life and freedom but also the credibility of formal law as a restraint on power. This supports the snare classification (victims include abstract goods). If no: the constraint targets only the human victims; the constitutional concept is merely damaged collateral. Changes how broadly we define the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_legitimacy_victim_extraction, conceptual, 'Whether constitutional legitimacy itself is extracted as a victim or merely damaged').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terror_coincidence__legitimation_during_purge_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terror_legit_theater_t0, terror_coincidence__legitimation_during_purge_reading, theater_ratio, 0, 0.88).
narrative_ontology:measurement(terror_legit_theater_t6, terror_coincidence__legitimation_during_purge_reading, theater_ratio, 6, 0.92).
narrative_ontology:measurement(terror_legit_theater_t12, terror_coincidence__legitimation_during_purge_reading, theater_ratio, 12, 0.95).

% Extraction over time
narrative_ontology:measurement(terror_legit_extract_t0, terror_coincidence__legitimation_during_purge_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(terror_legit_extract_t6, terror_coincidence__legitimation_during_purge_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement(terror_legit_extract_t12, terror_coincidence__legitimation_during_purge_reading, base_extractiveness, 12, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(terror_legit_suppress_t0, terror_coincidence__legitimation_during_purge_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(terror_legit_suppress_t6, terror_coincidence__legitimation_during_purge_reading, suppression_requirement, 6, 0.88).
narrative_ontology:measurement(terror_legit_suppress_t12, terror_coincidence__legitimation_during_purge_reading, suppression_requirement, 12, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terror_coincidence__legitimation_during_purge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(terror_coincidence__legitimation_during_purge_reading, terror_coincidence__plebiscitary_theater_reading).
narrative_ontology:affects_constraint(terror_coincidence__legitimation_during_purge_reading, terror_coincidence__sincere_blueprint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a single contested kernel (terror_coincidence). The sibling constraints are the plebiscitary_theater_reading and sincere_blueprint_reading — these are NOT separate constraints in the ε-invariance sense, but rather contesting interpretations of the same 1936 constitution. They share a kernel but diverge on the primary extractive/theatrical/sincere function. Each reading has its own story file with different emphasis and evidence base, but they refer to the same document and period. The network links record that these readings affect each other — accepting one constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(terror_coincidence__legitimation_during_purge_reading, analytical, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
