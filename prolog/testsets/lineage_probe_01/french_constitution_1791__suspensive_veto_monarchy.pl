% ============================================================================
% CONSTRAINT STORY: french_constitution_1791__suspensive_veto_monarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_constitution_1791__suspensive_veto_monarchy, []).

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
 *   constraint_id: french_constitution_1791__suspensive_veto_monarchy
 *   human_readable: The Suspensive Veto Monarchy (1791 Constitution)
 *   domain: political/constitutional/revolutionary
 *
 * SUMMARY:
 *   The 1791 Constitution's suspensive veto monarchy represents one unstable
 *   reading of how to transition from absolute to constitutional kingship.
 *   The Constitution of 1791 reformulated the French crown from sovereign
 *   source of all law to a hereditary magistrate with a two-legislature
 *   suspensive veto — the king could delay legislation but not prevent it
 *   permanently. This constraint embodies the constitutional commitment to
 *   retain monarchy while subordinating it to legislative supremacy and the
 *   people's representatives. The framework is one kernel (the 1791
 *   Constitution itself) read through multiple incompatible lenses. The
 *   suspensive veto reading instantiates a specific structural claim: that
 *   monarchy can be preserved by demoting kingship from sovereignty to
 *   executive function, and that the veto — a delaying power without
 *   permanent blocking capacity — represents both a genuine concession to
 *   monarchy and a successful constraint on absolute power. However, the
 *   historical trajectory reveals instability: the constraint endured only
 *   months before the flight to Varennes (June 1791) and the August 1792
 *   insurrection destroyed it entirely. The measurements show rising theater
 *   ratio (performative maintenance of the crown's dignity) and rising
 *   suppression requirement (the mechanism required increasing institutional
 *   effort to enforce as the fiction of limited monarchy became harder to
 *   sustain). This reading is one of four competing interpretations of the
 *   1791 kernel; the others address the citizenship split (active/passive),
 *   the prefixed declaration of rights (universal vs qualified), and the
 *   constitution's failure (discarded draft). Each reading has its own
 *   constraint story with its own ε, perspectives, and beneficiary/victim
 *   declarations.
 *
 * KEY AGENTS:
 *   - National Assembly: Primary beneficiary (organized/arbitrage) — retains legislative supremacy, controls the purse, exercises constituent power, can override the veto
 *   - Royal Sovereignty Doctrine: Primary victim (powerless/trapped) — the traditional absolute monarchy framework is constitutionally delegitimized; kingship is reformulated as subordinate executive function
 *   - Crown (Louis XVI as person): Mixed agent (powerful/constrained) — retains real executive power (appointment, command, foreign policy, veto mechanism) but experiences it as structurally constrained by constitutional limits and legislative override capacity
 *   - Legislative Supremacy (institutional principle): Primary beneficiary (organized/arbitrage) — the principle that law originates in the people's representatives and cannot be bound by hereditary monarchy becomes constitutionalized
 *   - Constitutional Reformers (Mounier, Malouet, monarchist constitutionalists): Framers (analytical/analytical) — see the suspensive veto as transitional bridge, believing monarchy will adapt to constitutional subordination or be gradually reformed
 *   - Absolute Monarchy Doctrine (natural law royalists): Secondary victim (institutional/trapped) — the claim that kingship is indivisible and immutable is contradicted by the 1791 framework; royalists see the constitution as logically impossible, not workable compromise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_constitution_1791__suspensive_veto_monarchy, 0.38).
domain_priors:suppression_score(french_constitution_1791__suspensive_veto_monarchy, 0.48).
domain_priors:theater_ratio(french_constitution_1791__suspensive_veto_monarchy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_constitution_1791__suspensive_veto_monarchy, extractiveness, 0.38).
narrative_ontology:constraint_metric(french_constitution_1791__suspensive_veto_monarchy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(french_constitution_1791__suspensive_veto_monarchy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_constitution_1791__suspensive_veto_monarchy, tangled_rope).
narrative_ontology:human_readable(french_constitution_1791__suspensive_veto_monarchy, "The Suspensive Veto Monarchy (1791 Constitution)").
narrative_ontology:topic_domain(french_constitution_1791__suspensive_veto_monarchy, "political/constitutional/revolutionary").

domain_priors:requires_active_enforcement(french_constitution_1791__suspensive_veto_monarchy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(french_constitution_1791__suspensive_veto_monarchy, '59b305a3-5f4c-499e-8aea-475b351d7e98').
narrative_ontology:cs_kernel_codification('59b305a3-5f4c-499e-8aea-475b351d7e98', formalized).
narrative_ontology:cs_authority_grounding('59b305a3-5f4c-499e-8aea-475b351d7e98', extraction).
narrative_ontology:cs_interpretation_layer_present('59b305a3-5f4c-499e-8aea-475b351d7e98').
narrative_ontology:cs_reading_relation('59b305a3-5f4c-499e-8aea-475b351d7e98', french_constitution_1791__active_passive_citizenship, influences).
narrative_ontology:cs_reading_relation('59b305a3-5f4c-499e-8aea-475b351d7e98', french_constitution_1791__declaration_of_rights_1789, coexists_with).
narrative_ontology:cs_reading_relation('59b305a3-5f4c-499e-8aea-475b351d7e98', french_constitution_1791__failure_and_succession, coexists_with).
narrative_ontology:cs_axiom('59b305a3-5f4c-499e-8aea-475b351d7e98', foundational, monarchy_reformable_through_constitutionalization).
narrative_ontology:cs_axiom_status(monarchy_reformable_through_constitutionalization, holdable).
narrative_ontology:cs_axiom_grounding('59b305a3-5f4c-499e-8aea-475b351d7e98', monarchy_reformable_through_constitutionalization, instrumental).
narrative_ontology:cs_axiom('59b305a3-5f4c-499e-8aea-475b351d7e98', secondary, veto_power_is_genuine_executive_constraint).
narrative_ontology:cs_axiom_status(veto_power_is_genuine_executive_constraint, overridden).
narrative_ontology:cs_axiom_grounding('59b305a3-5f4c-499e-8aea-475b351d7e98', veto_power_is_genuine_executive_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('59b305a3-5f4c-499e-8aea-475b351d7e98', constitutional_subordination_of_kingship).
narrative_ontology:cs_drift_state('59b305a3-5f4c-499e-8aea-475b351d7e98', august_1792_insurrection, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('59b305a3-5f4c-499e-8aea-475b351d7e98', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(french_constitution_1791__suspensive_veto_monarchy, french_constitution_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_constitution_1791__suspensive_veto_monarchy, national_assembly).
narrative_ontology:constraint_beneficiary(french_constitution_1791__suspensive_veto_monarchy, legislative_supremacy).
narrative_ontology:constraint_victim(french_constitution_1791__suspensive_veto_monarchy, royal_sovereignty_doctrine).
narrative_ontology:constraint_victim(french_constitution_1791__suspensive_veto_monarchy, absolute_monarchy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ROYAL SOVEREIGNTY DOCTRINE (SNARE) — The absolute monarchy's doctrinal foundation is structurally captured by the 1791 Constitution's reformulation. Kingship is transformed from sovereign source of law to magistrate executing legislation. No exit from this reframing without constitutional revision. The doctrine experiences maximum extraction: monarchy is nominally retained but its constitutional substance is delegitimized. Trapped at the biographical horizon — a reign cannot outlast the constitutional framework.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CROWN (TANGLED ROPE) — Louis XVI retains genuine executive power: appointment of ministers, military command, diplomatic authority, and the suspensive veto itself (a 2-legislature delay mechanism for legislation the king opposes). But this power is constrained and conditional — the veto can be overridden, the legislature controls the purse, and royal prerogative is subordinated to constitutional limits. Mixed experience: real coordination function (the veto provides constitutional braking) alongside asymmetric extraction (monarchy is demoted from sovereignty to subordination). Exit is constrained — abdication or flight risks civil war; compliance to the constitutional frame is mandatory.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NATIONAL ASSEMBLY (ROPE) — Experiences the suspensive veto as pure coordination: the king's 2-legislature delay provides a constitutional mechanism for executive review without preventing legislative will. The assembly benefits from primary authority (laws originate in the assembly, the budget is controlled by the assembly, the constitution itself was written by the assembly, not imposed by the king). The veto is a constraint but not extractive from the assembly's standpoint — it is a legitimate executive check. Net beneficiary position: arbitrage exit available (the assembly can revise the constitution, override the veto, or restructure executive power). The assembly sees this as successful constraint — monarchy has been subordinated to the rule of law.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CONSTITUTIONAL REFORMERS' VISION (SCAFFOLD) — From the perspective of the 1791 framers (Mounier, Malouet, and the monarchist constitutionalists), the suspensive veto is a temporary institutional bridge: a way to retain the crown's legitimacy and executive power while subordinating it to the people's representatives. The intent is that this framework will mature over generations into either a stable mixed constitution or a full legislative supremacy. The sunset logic is embedded in the constitutional text itself: the veto is not permanent royal prerogative but a delegated power that can be revised. Extraction is low because the framers see this as transitional — monarchy itself is being reformed, not extracted. The scaffold has explicit sunset pathways: either the crown adapts and becomes a modern constitutional monarch (British analogy), or the legislative majority abolishes/limits the veto further.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CEREMONIAL CROWN (PITON) — The suspensive veto, while structurally real, is often experienced as performance rather than function. Over the first two years (1791–1792), the king rarely uses the veto strategically; when he does, the assembly overrides or abandons the contested legislation anyway. The mechanism persists through institutional momentum rather than functional necessity. The monarchy's role becomes ceremonial and symbolic (the embodiment of the nation, the fountain of honor, the ceremonial executive) while the real power flows through the legislature. Theater ratio is elevated because much of the king's executive authority is ornamental — the legitimacy depends on retaining the crown's dignity and symbolic role, not on maximizing its practical veto power. The constraint endures through the performative maintenance of kingship.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW ABSOLUTISM (MOUNTAIN) — From the traditional natural law royalist perspective (held by monarchist critics of the 1791 Constitution), the crown's authority is not delegable and cannot be constitutionally demoted: absolute monarchy is the natural form of sovereignty, and any attempt to bind the king to law fundamentally misconceives the nature of kingship itself. From this view, the 1791 Constitution's suspensive veto monarchy is internally contradictory — it is a logical impossibility, not a workable compromise. The constraint appears as a mountain because the absolutist reading treats kingship as an immutable principle: you cannot have limited monarchy because the concept dissolves into either absolute monarchy or republic. However, this classification is a false summit: it naturalizes a contested doctrine as a law of nature. The structural data reveals that the 1791 Constitution is a contingent political artifact, not a law of nature or logic.
constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_constitution_1791__suspensive_veto_monarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_constitution_1791__suspensive_veto_monarchy, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_constitution_1791__suspensive_veto_monarchy, TR),
    TR >= 0.70.

:- end_tests(french_constitution_1791__suspensive_veto_monarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising to 0.45 by t12): Moderate. The suspensive veto mechanism creates asymmetric power because only the crown possesses it — the legislature cannot impose a reciprocal delay on the king. The extractiveness flows from the crown's retained (though limited) executive authority and the veto's delaying capacity. However, extractiveness is not high (0.45 is well below snare threshold of 0.46) because the veto is genuinely overridable and the legislature retains supreme authority over the budget, legislation, and constitutional revision. The rising trajectory reflects increasing strain: as political conflict intensifies (war declarations, treason investigations, religious conflicts), the veto becomes more frequently used and more politically contentious, driving extractiveness upward. By August 1792, the crowd's perception of the veto mechanism as a tool of royal resistance (rather than constitutional balance) shifted it toward pure snare mechanics, but this story captures the 1791 constitutional reading, not the August 1792 insurrection. Suppression (0.48, rising to 0.62): Moderate. The veto mechanism requires active institutional suppression of alternative frameworks (direct democracy, full republic without monarchy, pure legislative dictatorship). The crown must suppress the counterfactual narrative that absolute monarchy is legitimate; the assembly must suppress the narrative that monarchy is irreducible. Both suppress the narrative that the constitution is internally contradictory. Rising suppression reflects increasing cognitive dissonance as war, the flight, and political crises expose the framework's fragility. Theater ratio (0.55, rising to 0.68): Moderate-high. Much of the veto's function is performative: it embodies the symbolic dignity of the crown without commanding real blocking power in most situations. The ceremony of royal assent, the public use of the veto, and the maintenance of royal ritual around the constitutional structure carry significant theater. The rising trajectory reflects the increasing gap between the formal power the crown retains and its actual political capacity to exercise that power — by late 1791, the veto had become largely ceremonial (used for political signaling rather than legislative blockage).
 *
 * PERSPECTIVAL GAP:
 *   The suspensive veto monarchy classification varies radically across perspectives. The legislative assembly sees rope (pure coordination) because the veto functions as a constitutional check. The crown sees tangled_rope (mixed coordination and extraction) because real power is retained but strategically constrained. Royal sovereignty doctrine sees snare (pure extraction) because the doctrine itself is being deconstitutionalized. The constitutional reformers see scaffold (temporary bridge) because they believe the framework will mature into stable limited monarchy. The ceremonial crown sees piton (performative ritual) because much of the veto's exercise becomes ornamental as real politics move into the streets (war, the flight, the insurrection). The natural law absolutist sees mountain (logical impossibility) because kingship cannot be coherently divided. The perspectival gap reflects structural reality: the crown's constitutional position is unstable, the legislature's supremacy is conditional, and the entire frame rests on suppressed contradictions that the first year of operation begins to expose. No single perspective is 'wrong' — they are all accurate descriptions of different structural dimensions of an inherently contradictory institution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the veto constraint: whether they benefit (d low), bear costs (d high), or both (d mid-range). Legislative assembly as beneficiary + organized/arbitrage exit → low d (0.18), producing negative effective extraction χ (the assembly experiences the constraint as beneficial coordination). Crown as powerful agent + constrained exit → mid-range d (0.55), producing positive but moderate χ (the crown retains real power but cannot easily exit the constitutional frame without triggering civil conflict). Royal sovereignty doctrine as powerless victim + trapped exit → high d (0.92), producing high χ (the doctrine is structurally extracted from by the constitutional reframing). The constitutional reformers' scaffold perspective uses organized/constrained/analytical time horizon, producing d ≈ 0.35 and moderate χ (they see the constraint as transitional coordination, not extraction). The natural law absolutist uses the analytical context's canonical d ≈ 0.73, producing high χ, but reclassifies to mountain through the emerges_naturally gate (the absolutist claims kingship is a law of nature, not a contingent political artifact). The engine's false summit detector will flag this as naturalization of a contingent doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION VIA KERNEL READING: This constraint resolves mandatrophy by explicit kernel context. The 1791 Constitution is the kernel; the suspensive veto monarchy is one of four incompatible readings. Each reading has its own ε and classification. Mandatrophy is not 'which classification is correct?' but 'what structural claim does this particular reading of the kernel instantiate?' The suspensive veto reading claims that monarchy can be preserved through constitutional demotion — retained as magistrate, subordinated to law, constrained by the legislature. This is internally coherent as a single reading. Its collapse (August 1792) reflects not logical inconsistency within the reading itself but the reading's failure to accommodate the other three readings simultaneously. The 1791 Constitution could not sustain absolute rights (declaration), limited monarchy (veto), qualified citizenship (active/passive), AND temporary reform (failure/succession) in a single institutional frame. Each reading pulled toward its own logic. The veto reading's mandatrophy is resolved by recognizing it as one kernel reading among siblings; its extractiveness (0.38) and classification (tangled_rope) are accurate for THIS reading, not the whole constitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_override_rate_threshold,
    'At what frequency of veto override does the suspensive veto mechanism cease to function as genuine executive constraint and become performative delay?',
    'Historical measurement: count of royal vetoes, count of overrides, timeline of override decisions; comparison to European mixed constitutions (Belgium, Netherlands post-1830) where veto override rates are measured',
    'If override rate > 80%: veto is de facto performative; constraint reclassifies toward piton. If override rate < 20%: veto represents genuine legislative constraint; constraint remains tangled_rope. Threshold ambiguity affects whether the extractiveness (0.38) is accurate or understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_override_rate_threshold, empirical, 'Veto override frequency threshold for functional vs performative constraint').

omega_variable(
    royal_doctrine_resilience,
    'Can the crown maintain doctrinal legitimacy as a monarch if its constitutional substance is reduced to delaying power? Does monarchy require absolute or indivisible sovereignty to sustain its authority, or can it function as a subordinate executive?',
    'Comparison to later constitutional monarchies (Britain, Belgium, Spain): analysis of whether monarchical legitimacy survived or required adaptation; examination of royalist political theory before and after 1791 to assess doctrine rupture',
    'If doctrine cannot adapt: the 1791 Constitution is inherently unstable (which historical outcome confirmed). If doctrine can adapt: the suspensive veto monarchy represents a stable evolutionary path. This determines whether the snare classification of royal sovereignty doctrine is permanent or transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_doctrine_resilience, conceptual, 'Whether absolute sovereignty is necessary for monarchical legitimacy or contingent').

omega_variable(
    beneficiary_identity_ambiguity,
    'Who truly benefits from the suspensive veto monarchy: the National Assembly (legislative supremacy), the Crown (retained executive power), or the emerging bourgeoisie (property-rights protection through constitutional limitation of crown)?',
    'Analysis of legislative voting patterns, decree usage, and coalition formation 1791–1792; examination of whose interests were actually advanced (who gained property, access, office, or power); comparison to counterfactual scenarios (pure republic, continued absolute monarchy)',
    'If assembly benefits: current beneficiary declaration is correct. If crown benefits: veto should be reclassified as retention of real power, not demotion. If bourgeoisie benefits: beneficiary should be redeclared as property-rights class, not institutional actors. Affects directionality derivation and chi calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'True beneficiary of the suspensive veto mechanism').

omega_variable(
    constitutional_reading_contest,
    'Is the 1791 Constitution one coherent framework or four incompatible commitments (absolute rights, limited monarchy, property qualification, royal executive power) layered atop each other without resolution?',
    'Formal analysis of constitutional text for logical consistency; examination of whether the four sibling readings (active_passive_citizenship, declaration_of_rights, suspensive_veto, failure_and_succession) can coexist in one legal order or whether they form contradictions that forced the constitution''s collapse',
    'If coherent: constraint classification is accurate as written. If contradictory: the constraint should be decomposed into separate stories per sibling reading, each with its own kernel context and ε value. The current story assumes coherence but the historical evidence (one-year lifespan, internal collapse) suggests the constitution was unstable at its kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_reading_contest, conceptual, 'Whether the 1791 Constitution is logically coherent or internally contradictory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_constitution_1791__suspensive_veto_monarchy, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr1791_veto_theater_t0, french_constitution_1791__suspensive_veto_monarchy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fr1791_veto_theater_t6, french_constitution_1791__suspensive_veto_monarchy, theater_ratio, 6, 0.55).
narrative_ontology:measurement(fr1791_veto_theater_t12, french_constitution_1791__suspensive_veto_monarchy, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(fr1791_veto_extract_t0, french_constitution_1791__suspensive_veto_monarchy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fr1791_veto_extract_t6, french_constitution_1791__suspensive_veto_monarchy, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(fr1791_veto_extract_t12, french_constitution_1791__suspensive_veto_monarchy, base_extractiveness, 12, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fr1791_veto_suppress_t0, french_constitution_1791__suspensive_veto_monarchy, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fr1791_veto_suppress_t6, french_constitution_1791__suspensive_veto_monarchy, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(fr1791_veto_suppress_t12, french_constitution_1791__suspensive_veto_monarchy, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_constitution_1791__suspensive_veto_monarchy, enforcement_mechanism).
narrative_ontology:affects_constraint(french_constitution_1791__suspensive_veto_monarchy, french_constitution_1791__active_passive_citizenship).
narrative_ontology:affects_constraint(french_constitution_1791__suspensive_veto_monarchy, french_constitution_1791__declaration_of_rights_1789).
narrative_ontology:affects_constraint(french_constitution_1791__suspensive_veto_monarchy, french_constitution_1791__failure_and_succession).

% DUAL FORMULATION NOTE:
% The 1791 Constitution kernel decomposes into four structurally distinct constraint readings. The suspensive_veto_monarchy reading instantiates the claim that monarchy can be preserved through constitutional subordination. The sibling readings instantiate incompatible claims about citizenship qualification, universal rights, and institutional permanence. Each reading has its own constraint story with its own ε, perspectives, beneficiaries, and victims. The network edges indicate that the veto reading influences the citizenship reading (if monarchy is demoted, who exercises supreme power? answer: the active citizens), is influenced by the rights reading (universal rights delegitimize absolute monarchy), and coexists with the failure reading (the constitution did not mature as the framers intended; instead, it collapsed under the weight of its internal contradictions). All four stories must be read together to understand the 1791 kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
