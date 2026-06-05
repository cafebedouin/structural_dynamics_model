% ============================================================================
% CONSTRAINT STORY: german_basic_law__federal_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_basic_law__federal_construction, []).

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
 *   constraint_id: german_basic_law__federal_construction
 *   human_readable: German Basic Law Federal Construction (Reading: Federalism as Constitutional Eternity)
 *   domain: political/constitutional_law
 *
 * SUMMARY:
 *   The federal construction reading instantiates one core claim of the
 *   German Basic Law: federalism is constitutionally eternity-protected, and
 *   this protection is not merely institutional preference but a foundational
 *   commitment that binds all successors under Article 79(3). Länder possess
 *   constitutional status — they are not administrative subdivisions but
 *   co-legislators through the Bundesrat, with veto power over significant
 *   federal legislation. This reading emphasizes federalism as a structural
 *   constraint on centralization and a guarantor of regional difference. The
 *   constraint exhibits genuine coordination function (federal-state
 *   problem-solving requires negotiation; subsidiarity principle allocates
 *   decisions to appropriate levels) alongside asymmetric extraction (large
 *   states extract disproportionate power through majority dynamics; small
 *   states are locked into structural inferiority; uniform national policy
 *   speed is sacrificed to federal-state coordination overhead). The
 *   measurement trajectory shows rising extractiveness (0.28 → 0.38 over 76
 *   years) and rising suppression requirement (0.45 → 0.52), indicating that
 *   the federal mechanism has become increasingly costly to maintain as
 *   policy domains have become more complex and interdependent (EU
 *   integration, climate policy, pandemic response). The theater ratio has
 *   risen modestly (0.20 → 0.35), suggesting some increase in performative
 *   federal-state negotiation, but federalism remains substantially
 *   functional (unlike piton constraints where theater dominates). This
 *   constraint is one of five sibling readings of the contested kernel
 *   'german_basic_law'. The siblings offer alternative emphases:
 *   amendment_history highlights how the Basic Law has been repeatedly
 *   revised; basic_rights_catalog foregrounds rights protection;
 *   dignity_and_eternity centers on human dignity as the unamendable floor;
 *   militant_democracy emphasizes constitutional defense mechanisms. This
 *   reading, federal_construction, focuses specifically on how the Basic Law
 *   structures territorial power-sharing.
 *
 * KEY AGENTS:
 *   - Land Governments: Primary beneficiaries (organized/constrained) — constitute the Bundesrat, hold co-legislative veto power, preserve policy autonomy in education, police, culture
 *   - Large Federal States (Bavaria, Baden-Württemberg, NRW): Secondary beneficiaries (powerful/mobile) — extract disproportionate Bundesrat power through population weight and resource base; can form blocking coalitions
 *   - Small Länder (Bremen, Saarland): Primary victims (powerless/trapped) — nominally co-legislators but materially dependent on large-state coalitions; locked into structural inferiority by federalism design
 *   - Federal Executive & Bundestag: Constrained actors (powerful/mobile) — benefit from federalism's legitimacy and co-governance foundation, but constrained by Bundesrat veto requirements; must negotiate with Länder governments to pass legislation
 *   - Uniform National Policy Imperative: Abstract victim (powerless/trapped) — policy speed, regulatory coherence, and centralized coordination sacrificed to federal-state negotiation overhead
 *   - European Integration Pressure: Contextual actor (institutional/constrained) — creates new coordination demands on federal-state relationship; federalism now constrains Germany's EU negotiating capacity
 *   - Regional Difference & Subsidiarity: Normative beneficiary (institutional/arbitrage) — federalism institutionalizes the principle that decisions belong at lowest competent level; benefits from the constitutional guarantee against unitary centralization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_basic_law__federal_construction, 0.38).
domain_priors:suppression_score(german_basic_law__federal_construction, 0.52).
domain_priors:theater_ratio(german_basic_law__federal_construction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_basic_law__federal_construction, extractiveness, 0.38).
narrative_ontology:constraint_metric(german_basic_law__federal_construction, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(german_basic_law__federal_construction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_basic_law__federal_construction, tangled_rope).
narrative_ontology:human_readable(german_basic_law__federal_construction, "German Basic Law Federal Construction (Reading: Federalism as Constitutional Eternity)").
narrative_ontology:topic_domain(german_basic_law__federal_construction, "political/constitutional_law").

domain_priors:requires_active_enforcement(german_basic_law__federal_construction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(german_basic_law__federal_construction, 'd849cf56-de8b-48bb-a0a0-a813dd2ed487').
narrative_ontology:cs_kernel_codification('d849cf56-de8b-48bb-a0a0-a813dd2ed487', formalized).
narrative_ontology:cs_authority_grounding('d849cf56-de8b-48bb-a0a0-a813dd2ed487', lineage).
narrative_ontology:cs_interpretation_layer_present('d849cf56-de8b-48bb-a0a0-a813dd2ed487').
narrative_ontology:cs_reading_relation('d849cf56-de8b-48bb-a0a0-a813dd2ed487', german_basic_law__amendment_history, coexists_with).
narrative_ontology:cs_reading_relation('d849cf56-de8b-48bb-a0a0-a813dd2ed487', german_basic_law__basic_rights_catalog, influences).
narrative_ontology:cs_reading_relation('d849cf56-de8b-48bb-a0a0-a813dd2ed487', german_basic_law__dignity_and_eternity, influences).
narrative_ontology:cs_reading_relation('d849cf56-de8b-48bb-a0a0-a813dd2ed487', german_basic_law__militant_democracy, coexists_with).
narrative_ontology:cs_axiom('d849cf56-de8b-48bb-a0a0-a813dd2ed487', foundational, territorial_power_sharing_inviolable).
narrative_ontology:cs_axiom_status(territorial_power_sharing_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('d849cf56-de8b-48bb-a0a0-a813dd2ed487', territorial_power_sharing_inviolable, instrumental).
narrative_ontology:cs_axiom('d849cf56-de8b-48bb-a0a0-a813dd2ed487', foundational, land_constitutional_autonomy_eternity_protected).
narrative_ontology:cs_axiom_status(land_constitutional_autonomy_eternity_protected, holdable).
narrative_ontology:cs_axiom_grounding('d849cf56-de8b-48bb-a0a0-a813dd2ed487', land_constitutional_autonomy_eternity_protected, deontological).
narrative_ontology:cs_reference_frame('d849cf56-de8b-48bb-a0a0-a813dd2ed487', federal_coordinate_governance).
narrative_ontology:cs_drift_state('d849cf56-de8b-48bb-a0a0-a813dd2ed487', contemporary_eu_integration, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d849cf56-de8b-48bb-a0a0-a813dd2ed487', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(german_basic_law__federal_construction, german_basic_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_basic_law__federal_construction, land_governments).
narrative_ontology:constraint_beneficiary(german_basic_law__federal_construction, regional_difference_preservation).
narrative_ontology:constraint_victim(german_basic_law__federal_construction, uniform_national_policy_speed).
narrative_ontology:constraint_victim(german_basic_law__federal_construction, centralized_executive_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIFORM NATIONAL POLICY (SNARE) — Cannot exit the federalism constraint; bears full cost of coordination delays and veto points. Centralized policy coherence has no advocate within the constitutional structure itself. Maximum experienced extraction: policy speed sacrificed to Länder consent.
constraint_indexing:constraint_classification(german_basic_law__federal_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL GOVERNMENT EXECUTIVE (TANGLED ROPE) — Constrained by mandatory Bundesrat consent on concurrent legislation and significant issue areas. Genuine coordination function: federal-regional problem-solving requires negotiation. But also extraction: federal actors extract legislative veto power from smaller states through complex bargaining. Constrained exit (cannot unilaterally override, but can mobilize coalition support).
constraint_indexing:constraint_classification(german_basic_law__federal_construction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAND GOVERNMENTS COALITION (ROPE) — Primary beneficiaries of federalism eternity clause. Bundesrat membership guarantees legislative voice and veto on federal overreach. Experiences the constraint as coordination: power-sharing across territorial scales solves the problem of uniform dominance. Organized exit capacity (coalition formation, federal-state negotiation structures).
constraint_indexing:constraint_classification(german_basic_law__federal_construction, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL DIFFERENCE & SUBSIDIARITY (ROPE) — Federalism articulates the principle that decisions belong at the lowest competent level. This normative commitment benefits from the institutional guarantee: Länder retain policy autonomy in education, police, culture, local welfare. Low extraction cost because the mechanism is genuinely coordinative — it solves the problem of how to respect difference at scale. Arbitrage exit (can leverage the subsidiarity principle against federal incursion).
constraint_indexing:constraint_classification(german_basic_law__federal_construction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LARGE FEDERAL STATES (TANGLED ROPE) — Bavaria, Baden-Württemberg, North Rhine-Westphalia: large population and resource bases give them disproportionate Bundesrat power. Genuine coordination benefit (can block federal overreach). But also extractive: their size enables them to extract concessions on fiscal federalism and policy carve-outs. Mobile exit capacity (can form coalitions with federal government or other states; threaten legislative deadlock). Perspectival gap: large states see federal-state negotiation as power-sharing; small states see extraction.
constraint_indexing:constraint_classification(german_basic_law__federal_construction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SMALL LÄNDER (SNARE) — Nominally co-legislators via Bundesrat (equal vote regardless of population). Materially trapped: cannot sustain independent policy capacity; must follow large-state coalitions; bear costs of fiscal equalization without power over distribution terms. Federalism eternity clause locks them in. Zero exit capacity (cannot secede, cannot override coalition dynamics).
constraint_indexing:constraint_classification(german_basic_law__federal_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: EUROPEAN INTEGRATION PRESSURE (TANGLED_ROPE) — EU law, monetary union, and transnational governance create genuine coordination problems: Länder subsidiarity conflicts with EU supranational authority. Federal-state cooperation now required to negotiate EU commitments. But also extraction: federalism constrains Germany's negotiating speed in EU contexts; federal actors extract from Länder by requiring their consent to EU treaties, then use that consent to pressure Länder for domestic policy concessions. Constrained exit (cannot unilaterally leave federalism or EU without constitutional amendment).
constraint_indexing:constraint_classification(german_basic_law__federal_construction, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL/NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal vantage: any large polity requires territorial representation and power-sharing to prevent unitary dominance — federalism is a structural inevitability of scale. Article 79(3) eternity clause appears to ground this in immutable principle. However, the schema shows this is a FALSE SUMMIT: Land governments are identifiable beneficiaries; uniform policy speed is an identifiable victim. The 'immutable law of federalism' is actually a contingent institutional arrangement that benefits regional elites.
constraint_indexing:constraint_classification(german_basic_law__federal_construction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_basic_law__federal_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_basic_law__federal_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__federal_construction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(german_basic_law__federal_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The federal structure creates genuine coordination function (subsidiarity principle, power-sharing across territorial scales) alongside asymmetric extraction (large states extract power via Bundesrat dynamics; uniform policy speed is sacrificed). The constraint is not pure extraction (which would be 0.66+) because the coordination function is real and the mechanism has functional legitimacy. But it is not pure coordination (which would be ≤0.35) because identifiable beneficiaries (large states, Land governments) extract value from the structure while identifiable victims (small states, policy coherence) bear costs. Extractiveness has risen from 0.28 (1949, simpler policy environment, weaker EU integration) to 0.38 (2025, complex policy, strong EU integration), indicating that coordination overhead has increased. Suppression (0.52): Moderate-high. The constraint suppresses alternative institutional forms (unitary governance, majoritarian federal efficiency, rapid national policy responses) through the eternity clause (Article 79(3)) and constitutional structure. But suppression is not total (0.60+) because the constraint is transparent, legitimate, and subject to Länder negotiation. Exit from federalism is legally impossible (unamendable), but exit from particular federal-state agreements is constrained (not trapped). Theater ratio (0.35): Low-moderate. Federalism is substantially functional — the Bundesrat genuinely legislates, Länder governments actually negotiate, policy outcomes emerge from federal-state bargaining. But theater has increased over time as complexity has risen: federal-state conferences increasingly serve to coordinate pre-negotiated positions rather than genuine debate. The rise from 0.20 to 0.35 reflects this shift toward more performative negotiation.
 *
 * PERSPECTIVAL GAP:
 *   The federal construction reading produces dramatically different classifications across perspectives. Land governments see rope (coordination mechanism that solves power-sharing problem). Federal executive sees tangled_rope (genuine coordination but constrained by veto). Small Länder see snare (locked in with no exit capacity). Uniform policy imperative sees snare (sacrificed to coordination overhead). European integration context sees tangled_rope (creates new coordination demands while federalism constrains response speed). Large states see rope or light tangled_rope (power-sharing with advantage). The analytical observer risks seeing mountain (natural law inevitability) but schema reveals false summit. This perspectival diversity is NOT a sign of classification failure but a sign that the constraint has genuine structural complexity: it is simultaneously coordinative (solves territorial power-sharing problem) and extractive (benefits large states and Länder governments at cost of uniform policy speed).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by perspective based on structural position. Large state governments: beneficiaries with constrained (not trapped) exit → d ≈ 0.35-0.40 → moderate f(d) → experience federalism as power-sharing rope. Small state governments: trapped within the coalition structure yet formally co-legislators → d ≈ 0.65-0.75 → high f(d) → experience federalism as snare (locked in with no exit). Federal executive: both beneficiary (co-governs) and constrained victim (needs Bundesrat consent) → d ≈ 0.48-0.55 → moderate-high f(d) → experience federalism as tangled_rope. Uniform policy speed: abstract, organized-less victim with no structural exit → d ≈ 0.85-0.95 → maximum f(d) → perceives federalism as snare. The analytical observer from civilizational/universal scope risks classifying as mountain (federalism as inevitable law of large polities) but structural data reveals false summit: identifiable beneficiaries and victims show this is a contingent institutional arrangement, not a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The federal construction reading resolves mandatrophy by acknowledging that federalism is both coordinative and extractive simultaneously. The constraint creates genuine coordination function (solves territorial power-sharing problem; institutionalizes subsidiarity) AND asymmetric extraction (large states extract power; uniform policy sacrificed). This is the canonical tangled_rope signature: 0.30 ≤ ε ≤ 0.45, beneficiaries + victims + active enforcement all present. The constraint is NOT a false positive (not really rope misclassified as snare or vice versa) but a genuine hybrid. The perspectival gap is not a classification error but a structural feature: different agents genuinely experience the same mechanism as coordinative or extractive depending on their position. Land government experiences federalism as rope; small Länder experiences it as snare. Both are correct from their perspective. The mandatrophy asks: how do we prevent collapsing this to 'federalism is actually just extraction' or 'federalism is actually just coordination'? The answer: describe the presheaf. The constraint IS both, measured from different observer positions. Classifying uniformly as rope, snare, or tangled_rope would each obscure part of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternity_clause_bindingness,
    'Does Article 79(3) genuinely bind all successors, or is it a performative statement subject to reinterpretation under constitutional emergency?',
    'Constitutional theory analysis: comparison with other irreversible constitutional commitments globally; scenarios testing whether the clause would survive existential constitutional crisis (invasion, collapse of democratic process); judicial pronouncements on the clause''s legal force under stress',
    'If truly binding: federalism is locked in permanently; extractiveness remains at 0.38 because exit is impossible. If reinterpretable: federalism is contingent on political choice; extractiveness rises to 0.55+ because the eternity clause is performative theater masking the true constraint (federal veto negotiation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternity_clause_bindingness, conceptual, 'Whether Article 79(3) genuinely binds successors or is reinterpretable under constitutional emergency').

omega_variable(
    federal_state_asymmetry_inevitability,
    'Is the structural asymmetry between large and small Länder (disproportionate power) an unavoidable feature of federalism or a design choice that could be corrected?',
    'Comparative federalism: study equal-representation federations (US Senate model) versus proportional-influence federations (population-weighted). Institutional redesign analysis: what changes to Bundesrat voting rules would equalize power while maintaining federalism principle?',
    'If unavoidable: federalism necessarily extracts from small states; classification stable. If correctable: the current design is a contingent choice that benefits large states; reclassification threshold at 0.45+ because the extraction is not structurally required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_state_asymmetry_inevitability, empirical, 'Whether federal-state power asymmetry is inevitable or design-correctable').

omega_variable(
    reading_contention_federal_vs_rights,
    'If federal construction and basic rights catalog readings both claimed eternity-clause protection, would they conflict or coexist?',
    'Constitutional jurisprudence review: cases where Bundesrat veto blocked rights-protective federal legislation; cases where rights protection required overriding federal-state compromise. Test whether the two readings could both be maximally enforced within a single framework.',
    'If they conflict: federal construction reading FORECLOSES the rights-catalog reading at the eternity level (core premise of federalism — territorial power-sharing — contradicts core premise of absolute rights protection). If coexist: both readings holdable; no structural contradiction. This directly determines axioms and reading_relations structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_federal_vs_rights, conceptual, 'Conflict or coexistence between federal construction and basic rights readings at eternity level').

omega_variable(
    reunification_as_federalism_stress_test,
    'Did the 1990 reunification demonstrate that federalism can absorb massive structural shocks (East-West integration), or did it reveal federalism''s brittleness (need for special solidarity provisions, repeated Länder fiscal crisis)?',
    'Empirical study: longitudinal comparison of policy divergence East vs West Länder; fiscal transfer flows and equalization disputes; legislative deadlock frequency before and after reunification; qualitative assessment of whether federalism facilitated or hindered integration.',
    'If federalism proved robust: confidence in the reading increases; extractiveness assessment stable. If stress revealed brittleness: the eternity clause protects an increasingly fragile mechanism; extractiveness rises (coordination function degrades while veto power remains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reunification_as_federalism_stress_test, empirical, 'Whether reunification demonstrated federalism''s robustness or brittleness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_basic_law__federal_construction, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gblaw_fed_theater_1949, german_basic_law__federal_construction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gblaw_fed_theater_1991, german_basic_law__federal_construction, theater_ratio, 42, 0.28).
narrative_ontology:measurement(gblaw_fed_theater_2025, german_basic_law__federal_construction, theater_ratio, 76, 0.35).

% Extraction over time
narrative_ontology:measurement(gblaw_fed_extract_1949, german_basic_law__federal_construction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gblaw_fed_extract_1991, german_basic_law__federal_construction, base_extractiveness, 42, 0.35).
narrative_ontology:measurement(gblaw_fed_extract_2025, german_basic_law__federal_construction, base_extractiveness, 76, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gblaw_fed_supp_1949, german_basic_law__federal_construction, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gblaw_fed_supp_1991, german_basic_law__federal_construction, suppression_requirement, 42, 0.5).
narrative_ontology:measurement(gblaw_fed_supp_2025, german_basic_law__federal_construction, suppression_requirement, 76, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_basic_law__federal_construction, enforcement_mechanism).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, german_basic_law__amendment_history).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, german_basic_law__basic_rights_catalog).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, german_basic_law__dignity_and_eternity).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, german_basic_law__militant_democracy).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, eu_integration_pressure).
narrative_ontology:affects_constraint(german_basic_law__federal_construction, deutsch_bundestag_legislative_capacity).

% DUAL FORMULATION NOTE:
% The federal_construction reading is one of five sibling readings of the contested kernel 'german_basic_law'. All five readings interpret the same text (the Basic Law) but emphasize different structural commitments as foundational. The federal_construction reading structures federalism as the eternity-protected core; other readings emphasize rights protection (basic_rights_catalog, dignity_and_eternity), amendment history (amendment_history), or constitutional self-defense (militant_democracy). These are not competing descriptions of the same constraint but structurally distinct constraints derived from different readings of the kernel. Each sibling has its own extractiveness value, perspectives, and classification. All are linked via network.affects_constraints to show their mutual influence and contention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(german_basic_law__federal_construction, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
