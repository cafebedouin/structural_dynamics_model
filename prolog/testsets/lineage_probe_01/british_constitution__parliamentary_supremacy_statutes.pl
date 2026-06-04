% ============================================================================
% CONSTRAINT STORY: british_constitution__parliamentary_supremacy_statutes
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_constitution__parliamentary_supremacy_statutes, []).

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
 *   constraint_id: british_constitution__parliamentary_supremacy_statutes
 *   human_readable: Parliamentary Supremacy via Enactment: The British Constitution as Revisable Statute
 *   domain: political/legal/constitutional_authority
 *
 * SUMMARY:
 *   The British constitution, on this reading, is whatever the
 *   Crown-in-Parliament enacts via statute. No written document, no
 *   inalienable rights, no judicial override, no entrenchment: only the
 *   supremacy of Parliament expressed through legislation that can remade
 *   unions (Act of Union 1707), dissolved chambers (Parliament Act 1911),
 *   rearranged the monarchy (Act of Settlement 1701), and expanded the
 *   franchise (Representation of the People Acts). The doctrine suppresses
 *   every rival constitutional veto: Lords veto is overrideable (Parliament
 *   Act 1911), devolved bodies have no structural protection against
 *   Westminster unilaterally revoking their powers, a Bill of Rights (Human
 *   Rights Act 1998) can be repealed by bare majority. The beneficiary is the
 *   Commons majority of the day — the instant they win an election, they
 *   inherit constitutional supremacy. The victims are entrenchment-seekers
 *   (those who hoped to enshrine constitutional limits against future
 *   majorities) and historical constitutional limitations (charters,
 *   conventions, customary restraints that the doctrine treats as advisory at
 *   best). The extractiveness has risen over the interval 1688–1998 as the
 *   supremacy doctrine became formalized through statute rather than
 *   remaining implicit, and as successive majorities demonstrated willingness
 *   to use it against rival constitutional claims. Theater has risen because
 *   the formal procedures of parliamentary legislation (committee stages,
 *   Lords consideration, royal assent) persist as ceremony while the
 *   majority's substantive power over constitutional substance has become
 *   unrestricted. Suppression has risen because the doctrine logically closes
 *   every formal avenue for entrenchment — there is no constitutional court,
 *   no supermajority requirement, no referential authority outside Parliament
 *   itself that could enforce a limit against a determined majority.
 *
 * KEY AGENTS:
 *   - Commons Majority of the Day (institutional/arbitrage): Primary beneficiary — inherits full constitutional power upon election victory; can revise any constitutional arrangement via statute.
 *   - Entrenchment-Seekers (powerless/trapped): Primary victims — seek to enshrine limits but find every formal mechanism of entrenchment declared revisable by bare majority.
 *   - Opposition Party (moderate/constrained): Secondary victim — participates in parliamentary process but can reverse majority decisions only by winning a majority themselves.
 *   - Constitutional Reform Coalition (organized/constrained): Organized actors attempting to create de facto entrenchment via convention (Salisbury Convention) or embedded veto-holders (devolved bodies, Lords reform).
 *   - Devolved Legislatures (institutional/constrained): Theoretically subordinate to Westminster supremacy; lack formal constitutional protection against unilateral Westminster override.
 *   - Historical Constitutional Traditions (analytical/analytical): Charters, conventions, customary restraints treated as non-binding or advisory under the supremacy doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_constitution__parliamentary_supremacy_statutes, 0.52).
domain_priors:suppression_score(british_constitution__parliamentary_supremacy_statutes, 0.68).
domain_priors:theater_ratio(british_constitution__parliamentary_supremacy_statutes, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_constitution__parliamentary_supremacy_statutes, extractiveness, 0.52).
narrative_ontology:constraint_metric(british_constitution__parliamentary_supremacy_statutes, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(british_constitution__parliamentary_supremacy_statutes, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_constitution__parliamentary_supremacy_statutes, tangled_rope).
narrative_ontology:human_readable(british_constitution__parliamentary_supremacy_statutes, "Parliamentary Supremacy via Enactment: The British Constitution as Revisable Statute").
narrative_ontology:topic_domain(british_constitution__parliamentary_supremacy_statutes, "political/legal/constitutional_authority").

domain_priors:requires_active_enforcement(british_constitution__parliamentary_supremacy_statutes).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_constitution__parliamentary_supremacy_statutes, '17ce789c-f5ca-46ac-a69c-5433ae2cd828').
narrative_ontology:cs_kernel_codification('17ce789c-f5ca-46ac-a69c-5433ae2cd828', formalized).
narrative_ontology:cs_authority_grounding('17ce789c-f5ca-46ac-a69c-5433ae2cd828', extraction).
narrative_ontology:cs_interpretation_layer_present('17ce789c-f5ca-46ac-a69c-5433ae2cd828').
narrative_ontology:cs_reading_relation('17ce789c-f5ca-46ac-a69c-5433ae2cd828', british_constitution__constitutional_conventions, coexists_with).
narrative_ontology:cs_reading_relation('17ce789c-f5ca-46ac-a69c-5433ae2cd828', british_constitution__foundational_charters, coexists_with).
narrative_ontology:cs_reading_relation('17ce789c-f5ca-46ac-a69c-5433ae2cd828', british_constitution__modern_judicialization, influences).
narrative_ontology:cs_reading_relation('17ce789c-f5ca-46ac-a69c-5433ae2cd828', british_constitution__revolution_settlement, coexists_with).
narrative_ontology:cs_axiom('17ce789c-f5ca-46ac-a69c-5433ae2cd828', foundational, parliament_legislative_supremacy).
narrative_ontology:cs_axiom_status(parliament_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('17ce789c-f5ca-46ac-a69c-5433ae2cd828', parliament_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('17ce789c-f5ca-46ac-a69c-5433ae2cd828', foundational, entrenchment_impossibility).
narrative_ontology:cs_axiom_status(entrenchment_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('17ce789c-f5ca-46ac-a69c-5433ae2cd828', entrenchment_impossibility, empirically_contingent).
narrative_ontology:cs_reference_frame('17ce789c-f5ca-46ac-a69c-5433ae2cd828', parliamentary_legislative_supremacy_framework).
narrative_ontology:cs_drift_state('17ce789c-f5ca-46ac-a69c-5433ae2cd828', contemporary_post_devolution_judicialization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('17ce789c-f5ca-46ac-a69c-5433ae2cd828', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(british_constitution__parliamentary_supremacy_statutes, british_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_constitution__parliamentary_supremacy_statutes, commons_majority_of_day).
narrative_ontology:constraint_beneficiary(british_constitution__parliamentary_supremacy_statutes, executive_in_majority_coalition).
narrative_ontology:constraint_victim(british_constitution__parliamentary_supremacy_statutes, entrenchment_seekers).
narrative_ontology:constraint_victim(british_constitution__parliamentary_supremacy_statutes, historical_constitutional_limitations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: ENTRENCHMENT-SEEKER (SNARE) — Any faction seeking to enshrine constitutional limits finds every traditional veto suppressed. The supremacy doctrine holds that Parliament of today can undo what Parliament of yesterday entrenched. Bills of Rights, devolution settlements, separation of powers — all declared revisable by bare majority. Maximum extraction: the seeking of entrenchment is itself criminalized in constitutional principle. No exit option; cannot escape the supremacy of the sitting Commons.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: OPPOSITION PARTY (TANGLED ROPE) — Experiences coordination function: Parliament coordinates parliamentary behavior via statute; opposition participates in making statute and can reverse it when majority shifts. Also experiences extraction: current majority can lock in changes via statute that are costly for opposition to reverse (e.g., boundary changes, devolution rollback, electoral system changes). Constrained: real parliamentary channels exist, but re-winning the majority is the only escape.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: GOVERNING COMMONS MAJORITY (ROPE) — Experiences pure coordination: the supremacy statute doctrine enables them to enact legislative will into constitutional effect. Arbitrage exit: majority can revise own decisions; doctrine is transparent to their interests. Net beneficiary — the supremacy principle transfers constitutional power to legislative numbers, which is their domain.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (Lords reform movements, devolution architects, rights-instrument advocates) see the supremacy doctrine as a temporary problem with structured countermeasures. The coalition's exit strategy is de facto entrenchment via supermajority convention (Salisbury Convention, fixed-term parliaments) or embedded veto-holders (devolved legislatures, Lords). These are not formal legal limits but structural constraints that make bare-majority revision costly and rare. The sunset is gradual normalization of convention-as-effective-entrenchment.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: THE LEGISLATIVE RITUAL (PITON) — The day-to-day enactment of statutes through parliamentary procedure is largely ceremonial relative to the supremacy doctrine's operative force. The ritual (royal assent, committee stages, Lords amendments, formal passage) performs constitutional legitimacy but does not constrain the majority's substantive power. The theater_ratio reflects that the formal procedures of making a statute have drifted from their function (deliberation, cooling-off, second thought) into pure ceremony. Majorities bypass standing orders; the Lords is overrideable; the ritual persists through institutional inertia, not because it works as originally designed.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 6: ANALYTICAL OBSERVER — LOGICAL NECESSITY (MOUNTAIN) — From a pure logical standpoint, if a constitution is law, and law is whatever the supreme legislature enacts, then the constitution must be whatever that legislature enacts. Entrenchment is logically impossible: no legislature can bind its successor. The supremacy of Parliament is presented as a necessary truth about sovereignty itself, not a contingent institutional arrangement. However, this naturalizes a choice: that Parliament is the supreme constitutional authority AT ALL is contested. The engine's false summit detector will flag this perspective as naturalization of what is actually a particular institutional reading.
constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_constitution__parliamentary_supremacy_statutes_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_constitution__parliamentary_supremacy_statutes, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(british_constitution__parliamentary_supremacy_statutes, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_constitution__parliamentary_supremacy_statutes, TR),
    TR >= 0.70.

:- end_tests(british_constitution__parliamentary_supremacy_statutes_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The supremacy doctrine gives the Commons majority of the day absolute constitutional power to revise the entire governance framework. This is extraction for entrenchment-seekers (victims who find their constitutional aspirations declared revisable) and for opposition parties (who face constitutional changes locked in by the majority they must reverse). But extractiveness is not maximal (≥0.66 snare level) because the Commons majority itself experiences genuine coordination benefits — they can legislate constitutional matters directly without supermajority requirement, judicial override, or referendum. The constraint both extracts from entrenchment-seekers and enables coordination for the governing majority. Suppression (0.68): High. Every formal rival veto is suppressed: the Lords cannot block statutory changes to the constitution (Parliament Acts), devolved bodies cannot claim domain supremacy (Westminster can revoke devolution), courts cannot declare statutes unconstitutional (no judicial review of parliamentary sovereignty), and the people cannot entrench rights via referendum (Parliament can revoke even a rights act via bare majority). The suppression is structural and formal — not just practical but legal. Theater ratio (0.55): Moderate. The formal procedures of parliamentary legislation (committee stages, Lords consultation, royal assent) continue to perform deliberative and legitimacy-conferring functions — they are not purely ceremonial. However, the theater has risen over time because these procedures no longer serve their original functions of constraint and deliberation: the majority can guillotine debate, override Lords objections, and ram through constitutional change in a single session. The ritual persists in form while its substance erodes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a striking perspectival gap. The governing Commons majority sees pure coordination (Rope) — the supremacy doctrine enables them to translate legislative numbers into constitutional authority. The opposition sees mixed extraction and coordination (Tangled Rope) — they can participate in making statute and reverse the majority's choices only by winning a future majority. Entrenchment-seekers see pure extraction (Snare) — every institutional mechanism for entrenchment is declared revisable, and their attempts to enshrine limits are criminalized in constitutional principle. The constitutional reform coalition sees a degraded system that is salvageable through convention and embedded veto-holders (Scaffold) — a sunset into de facto entrenchment via supermajority convention. The legislative ritual itself is performative (Piton) — the formal procedures continue but their function (deliberation, second thought, constraint) has atrophied. The analytical observer from a universal/civilizational perspective risks seeing logical necessity (Mountain) — if law is what the supreme legislature enacts, then entrenchment is impossible in principle. But this naturalizes a choice: that Parliament is supreme AT ALL is the contested reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from the agent's structural position relative to the supremacy constraint. The beneficiary (Commons majority) has d ≈ 0.15 (strong beneficiary) — arbitrage exit means they experience negative effective extraction; the doctrine amplifies their interests. The opposition has d ≈ 0.60 (moderate extraction target) — they have real parliamentary voice and can reverse decisions via electoral victory, but currently bear costs of the majority's constitutional choices. Entrenchment-seekers have d ≈ 0.90 (near-total extraction target) — trapped, with no formal mechanism to protect their constitutional aspirations. The commons majority's institutional perspective applies canonical d ≈ 0.00 for the power atom (institutional) with arbitrage exit, producing minimal experienced chi. The opposition's moderate perspective applies d ≈ 0.65, producing moderate chi. The entrenchment-seeker's powerless perspective applies d ≈ 1.00 with trapped exit, producing maximum chi. No directionality overrides are necessary; the derivation from beneficiary/victim status and exit options produces the correct perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that parliamentary supremacy has real coordination functions (enabling legislation of constitutional matters directly, without supermajority requirement or referendum) AND real extractive asymmetries (locking in constitutional changes that entrenchment-seekers cannot protect against, giving the majority de facto veto over constitutional limits). The Tangled Rope classification captures both: genuine coordination (beneficiaries participate in making the supreme law) alongside asymmetric extraction (victims find every entrenchment mechanism declared revisable). The Piton classification of the legislative ritual reflects that the formal procedures of parliamentary passage have lost their deliberative function while persisting through institutional inertia. The Scaffold classification of the constitutional reform coalition reflects genuine counter-movements (devolution, rights instruments, convention-based entrenchment) that are structurally real even if legally reversible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_vs_entrenchment_threshold,
    'At what threshold does a convention (the Salisbury Convention, the convention against dissolving Parliament mid-term before the Fixed Term Parliaments Act) become functionally equivalent to entrenchment, and does functional entrenchment contradict the parliamentary supremacy doctrine?',
    'Historical analysis of convention-breaking attempts and their costs: if majority repeatedly attempts to break conventions and incurs political damage sufficient to deter repetition, the convention is functionally entrenching despite legal non-bindingness. If majority can break convention costlessly, the supremacy doctrine holds in practice.',
    'If conventions are functionally entrenching: the doctrine''s claim of bare-majority revisability is theoretical, not structural. Classification shifts toward Scaffold from more perspectives. If not: the supremacy doctrine operates as claimed; Snare classification confirmed from entrenchment-seeker perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convention_vs_entrenchment_threshold, empirical, 'Functional equivalence of convention to entrenchment').

omega_variable(
    devolved_sovereignty_recursion,
    'Do devolved legislatures (Scottish Parliament, Welsh Assembly, Northern Irish Assembly) hold genuine legislative supremacy within their domains, or do they remain subordinate to Westminster supremacy — and if subordinate, does the supremacy doctrine''s claim to absolute power suppress rival constitutional centers that would otherwise have standing?',
    'Legal analysis of devolution acts: are they structurally revisable by Westminster alone, or do they embed consultation/consent requirements? Empirical analysis of devolution disputes: how many times has Westminster overridden devolved decisions, and what was the political cost?',
    'If devolved legislatures hold genuine domain supremacy: the parliamentary supremacy doctrine applies only within UK scope, and we have a multi-sovereign constitution. If Westminster can unilaterally override: the doctrine suppresses rival constitutional claims (supporting Snare classification for devolved actors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devolved_sovereignty_recursion, conceptual, 'Devolved legislatures as rival sovereignty centers vs Westminster supremacy').

omega_variable(
    supremacy_vs_legitimacy_gap,
    'Does the supremacy doctrine describe the actual source of constitutional legitimacy in British practice, or does it describe only the legal form while legitimacy derives from (unacknowledged) convention, historical settlement, or public assent?',
    'Comparative case analysis: instances where Westminster commanded majorities had legal supremacy but lacked political legitimacy (Irish Home Rule, attempted dissolution of the Scottish Parliament, devolution referenda). If legitimacy departs from supremacy, what actually constrains the majority?',
    'If legitimacy derives from sources other than supremacy (convention, historical precedent, devolved consent): the doctrine is a false summit — it describes form while other structures do the constraining work. If legitimacy follows supremacy: the doctrine''s extractive force is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_vs_legitimacy_gap, conceptual, 'Gap between legal supremacy and actual legitimacy sources').

omega_variable(
    reading_identity_kernel,
    'This constraint instantiates a reading of the contested British constitution kernel. The reading claims that the constitution is whatever Crown-in-Parliament enacts via statute. But does this reading describe a natural feature of sovereignty (no sovereign body can be constitutionally bound) or a politically contingent choice about which body gets to claim supremacy (why Parliament, not Crown alone, not people via referendum)?',
    'Historical analysis: the supremacy doctrine was not stated a priori but emerged through the Revolution Settlement (1688-1701) and was codified through later supremacy statutes (Act of Union 1707, Parliament Act 1911, etc.). If the doctrine emerged as a contingent institutional arrangement, its claim to logical necessity is unsupported.',
    'If contingent: the reading''s grounding_type shifts from logical necessity to conventional/historical. If necessary: the reading is harder to dislodge. The sibling reading (revolution_settlement) locates this constraint''s origin in a specific historical moment, suggesting contingency rather than necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_kernel, conceptual, 'Whether supremacy doctrine is logically necessary or historically contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_constitution__parliamentary_supremacy_statutes, 1688, 1998).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brit_parl_theater_1688, british_constitution__parliamentary_supremacy_statutes, theater_ratio, 1688, 0.35).
narrative_ontology:measurement(brit_parl_theater_1832, british_constitution__parliamentary_supremacy_statutes, theater_ratio, 1832, 0.45).
narrative_ontology:measurement(brit_parl_theater_1911, british_constitution__parliamentary_supremacy_statutes, theater_ratio, 1911, 0.5).
narrative_ontology:measurement(brit_parl_theater_1998, british_constitution__parliamentary_supremacy_statutes, theater_ratio, 1998, 0.55).

% Extraction over time
narrative_ontology:measurement(brit_parl_extractiveness_1688, british_constitution__parliamentary_supremacy_statutes, base_extractiveness, 1688, 0.15).
narrative_ontology:measurement(brit_parl_extractiveness_1832, british_constitution__parliamentary_supremacy_statutes, base_extractiveness, 1832, 0.35).
narrative_ontology:measurement(brit_parl_extractiveness_1911, british_constitution__parliamentary_supremacy_statutes, base_extractiveness, 1911, 0.48).
narrative_ontology:measurement(brit_parl_extractiveness_1998, british_constitution__parliamentary_supremacy_statutes, base_extractiveness, 1998, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(brit_parl_suppression_1688, british_constitution__parliamentary_supremacy_statutes, suppression_requirement, 1688, 0.4).
narrative_ontology:measurement(brit_parl_suppression_1832, british_constitution__parliamentary_supremacy_statutes, suppression_requirement, 1832, 0.55).
narrative_ontology:measurement(brit_parl_suppression_1911, british_constitution__parliamentary_supremacy_statutes, suppression_requirement, 1911, 0.65).
narrative_ontology:measurement(brit_parl_suppression_1998, british_constitution__parliamentary_supremacy_statutes, suppression_requirement, 1998, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_constitution__parliamentary_supremacy_statutes, enforcement_mechanism).
narrative_ontology:affects_constraint(british_constitution__parliamentary_supremacy_statutes, british_constitution__constitutional_conventions).
narrative_ontology:affects_constraint(british_constitution__parliamentary_supremacy_statutes, british_constitution__foundational_charters).
narrative_ontology:affects_constraint(british_constitution__parliamentary_supremacy_statutes, british_constitution__modern_judicialization).
narrative_ontology:affects_constraint(british_constitution__parliamentary_supremacy_statutes, british_constitution__revolution_settlement).

% DUAL FORMULATION NOTE:
% The British constitution kernel decomposes into five distinct constraint stories, each a reading of the same contested claim: what is the source and nature of British constitutional authority? This story (parliamentary_supremacy_statutes) represents the reading that claims the supremacy statutes are the real constitutional text. Each sibling reading has its own ε value reflecting the empirical status of that reading's core claim. They are linked via network.affects_constraints to show the constraint family structure. A change in one reading's classification or extractiveness affects the standing of its siblings — for example, if judicial review becomes entrenched (affecting modern_judicialization), the parliamentary supremacy reading loses some of its claimed unchecked power. Each reading is authored as a complete constraint story conforming to the DR framework, with its own perspectives, beneficiary/victim declarations, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
