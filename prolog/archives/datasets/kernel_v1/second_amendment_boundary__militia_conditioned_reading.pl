% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Boundary (Militia-Conditioned Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The militia-conditioned reading of the Second Amendment treats the
 *   prefatory clause 'A well regulated Militia, being necessary to the
 *   security of a free State' as a substantive limit on the operative clause
 *   'the right of the people to keep and bear Arms, shall not be infringed.'
 *   Under this reading, the constitutional right to bear arms is not an
 *   individual right but a collective right dependent on militia service or
 *   state-authorized defense function. This reading was the dominant
 *   constitutional interpretation for most of American history (settled law
 *   from the founding through DC v. Heller 2008) and remains the position of
 *   many legal scholars, state attorneys general, and public-safety
 *   advocates. The reading confers regulatory authority on states to impose
 *   comprehensive firearms restrictions (licensing, registration, category
 *   bans, ammunition taxation) on the presumption that such regulations serve
 *   public safety within the militia-conditioned constitutional frame. The
 *   constraint instantiates a committer choice: the decision to weight the
 *   prefatory clause as limiting rather than merely motivating transforms the
 *   constitutional meaning from an individual right (potentially subject to
 *   strict scrutiny) into a collectivized right (subject to rational-basis
 *   deference to state authority). This is one reading of a contested kernel
 *   — the kernel being the Second Amendment text itself, which admits
 *   multiple coherent readings depending on how the interpreter treats the
 *   grammatical relationship between the prefatory and operative clauses.
 *
 * KEY AGENTS:
 *   - State Regulatory Authority: Primary beneficiary (institutional/arbitrage) — militia-conditioned reading legitimizes comprehensive firearms regulation; states can implement diverse regulatory regimes within rational-basis frame
 *   - Gun Owner in High-Regulation Jurisdiction: Primary victim (powerless/trapped) — cannot exit jurisdiction and has no legal recourse to challenge regulations under rational-basis scrutiny
 *   - Organized Gun Rights Coalition: Secondary actor (organized/constrained) — benefits from constitutional framing (mobilizes identity and resources) but faces suppression through narrowed individual-right status; can organize across jurisdictions and litigate
 *   - Firearms Industry & Commerce Sector: Secondary victim (institutional/constrained) — faces regulatory fragmentation and state-imposed market splits but benefits from constitutional legitimacy of the right itself
 *   - Public Health & Safety Coalition: Secondary beneficiary (institutional/arbitrage) — reads militia-conditioned interpretation as enabling evidence-based regulation and validating public-health interests
 *   - Originalist Textualist Judiciary: Institutional degradation (institutional/constrained) — forced into performative engagement with 18th-century militia concepts that are texturally inaccessible; maintains constraint through stare decisis despite rational erosion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.52).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.68).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Boundary (Militia-Conditioned Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, 'b9377e62-e8f7-46d2-bea0-aca39a3f429d').
narrative_ontology:cs_kernel_codification('b9377e62-e8f7-46d2-bea0-aca39a3f429d', formalized).
narrative_ontology:cs_authority_grounding('b9377e62-e8f7-46d2-bea0-aca39a3f429d', lineage).
narrative_ontology:cs_interpretation_layer_present('b9377e62-e8f7-46d2-bea0-aca39a3f429d').
narrative_ontology:cs_reading_relation('b9377e62-e8f7-46d2-bea0-aca39a3f429d', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9377e62-e8f7-46d2-bea0-aca39a3f429d', second_amendment_boundary__insurrectionist_reading, influences).
narrative_ontology:cs_axiom('b9377e62-e8f7-46d2-bea0-aca39a3f429d', foundational, prefatory_clause_limits_operative).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative, holdable).
narrative_ontology:cs_axiom_grounding('b9377e62-e8f7-46d2-bea0-aca39a3f429d', prefatory_clause_limits_operative, empirically_contingent).
narrative_ontology:cs_axiom('b9377e62-e8f7-46d2-bea0-aca39a3f429d', foundational, state_regulatory_authority_presumed_legitimate).
narrative_ontology:cs_axiom_status(state_regulatory_authority_presumed_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b9377e62-e8f7-46d2-bea0-aca39a3f429d', state_regulatory_authority_presumed_legitimate, deontological).
narrative_ontology:cs_reference_frame('b9377e62-e8f7-46d2-bea0-aca39a3f429d', collective_militia_authority_framework).
narrative_ontology:cs_drift_state('b9377e62-e8f7-46d2-bea0-aca39a3f429d', post_heller_doctrine_shift, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b9377e62-e8f7-46d2-bea0-aca39a3f429d', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, unrestricted_gun_ownership_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_commerce_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUN OWNER IN HIGH-REGULATION JURISDICTION (SNARE) — Individual claiming right to own firearms for self-defense or collection faces comprehensive regulatory barriers (licensing, registration, storage requirements, category bans, ammunition taxes). Under militia-conditioned reading, these restrictions are presumed constitutional if rational-basis justification exists. The gun owner cannot exit (jurisdiction-locked by residence/career) and has no legal recourse to challenge specifics of regulations that survive rational-basis scrutiny. Full extraction — the constitutional reading eliminates the individual's claim before adjudication.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED GUN RIGHTS COALITION (TANGLED ROPE) — Second Amendment advocacy organizations (NRA, 2A sanctuaries) benefit from constitutional mobilization (frames their cause as rights-based) but face suppression through the militia-conditioned reading's demotion of individual-right status. They have agency (can litigate, organize, lobby) and exit paths (coalition-building in sympathetic jurisdictions, federal override campaigns) but face significant costs. The constraint coordinates their identity (constitutional grievance frame) while extracting through narrowed legal standing.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY APPARATUS (ROPE) — Jurisdictions enacting firearms regulations under militia-conditioned reading experience the constraint as pure coordination: the reading legitimizes state action and provides the rational-basis standard that permits varied regulatory regimes. States can implement anything from strict licensing (California, New York) to permitless carry (permitless carry states reading the same clause differently). The reading COORDINATES state authority — it tells states they have regulatory power. Net beneficiary — the constraint enables state action.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FIREARMS INDUSTRY (TANGLED ROPE) — Gun manufacturers, dealers, and ammunition producers benefit from the legitimacy that the Second Amendment provides (even under militia-conditioned reading, the amendment preserves some right) and from market demand across jurisdictions. But they face suppression through regulatory fragmentation: militia-conditioned reading empowers state legislatures to impose differential regulations (category bans, licensing requirements, commerce restrictions). The industry must navigate 50+ different regulatory regimes and arbitrage market splits. Mixed position: constitutional legitimacy (benefit) + regulatory suppression (cost).
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH & SAFETY COALITION (ROPE) — Gun violence reduction advocates experience the militia-conditioned reading as enabling coordination for public health policy. The reading validates state authority to impose health-based regulations (waiting periods, background checks, red-flag laws) and frames these as rational policy rather than constitutional overreach. The coalition benefits from the reading's legitimacy framework — it coordinates around the presumption that public health is a valid state interest that can justify regulation. Net beneficiary.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORIGINALIST TEXTUALIST JUDICIARY (PITON) — Judges committed to originalist interpretation face a degraded institutional position under militia-conditioned reading. The reading requires engaging 18th-century militia context, but that context is theatrically inaccessible: no judge can definitively establish whether the founders intended purely collective militia service or included individual self-defense components. The originalist project becomes performative — claiming fidelity to founding intent while necessarily making judgments about contested historical claims. The theater persists because judicial deference to established precedent (stare decisis) maintains the constraint despite erosion of its rational foundations.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the militia-conditioned reading appears to reflect an immutable structural feature: constitutional language always requires interpretation, and the plain text of the Second Amendment (if its prefatory clause truly conditions the operative clause) cannot be read any other way — the constraint is built into language itself. However, this naturalizes what is actually a reading choice (the decision to weight the prefatory clause as limiting vs. merely motivating). The engine's false-summit detector should identify this as a false mountain.
constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_boundary__militia_conditioned_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The militia-conditioned reading extracts from gun owners by presuming state regulatory authority and shifting the burden of proof to those claiming an individual right. In high-regulation jurisdictions, comprehensive restrictions (registration, licensing, category bans) are presumed constitutional if any rational basis exists. However, the extraction is not maximum because: (1) some states choose minimal regulation even under this reading (rational-basis permits latitude); (2) the firearms industry benefits from the constitutional legitimacy of the right itself; (3) public-health advocates also experience benefits. The measurement trajectory (0.15 → 0.52 over 100-year interval, peaking at post-Heller doctrine drift) reflects increasing extractiveness as courts clarify militia-conditioned requirements and states layer regulations. The rise from founding (militia-conditioned was settled law, but extractiveness was modest because most citizens were male and militia service was expected) to modern era (extractiveness rises as regulations target specific populations and categories, and as the Heller decision creates doctrinal friction) shows extraction accumulation. Suppression (0.68): High. Gun owners in restrictive jurisdictions face multiple suppression mechanisms: legal barriers (licensing, registration mandates), economic barriers (category bans that eliminate entire market segments), mobility barriers (cannot exit jurisdiction without relocation), and epistemic barriers (cannot legally challenge regulations under rational-basis scrutiny). The suppression is structural, not internalized — gun owners in high-regulation states face real material barriers. Theater ratio (0.58): Moderate-high. Militia-conditioned reading requires courts to engage 18th-century militia context, but that context is theatrically inaccessible: no judge can definitively establish founding-era intent regarding the prefatory clause's limiting function. Courts must perform historical recovery (produce historical narratives) while acknowledging hermeneutic uncertainty. The theater increased over the measurement interval as originalist methodology became more elaborate and more clearly performative (Heller's originalism required selecting among contested historical sources).
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests radically different classifications across perspectives, revealing how the militia-conditioned reading operates as a control point for access to constitutional meaning. The gun owner in a high-regulation jurisdiction perceives a snare — extraction without exit or remedy. The organized gun-rights coalition perceives tangled rope — constitutional identity and mobilization capacity balanced against suppression. The state perceives pure rope — coordination enabled, no extraction experienced. The firearms industry perceives tangled rope — legitimacy benefit offset by regulatory fragmentation. The public-health coalition perceives rope — state authority coordinated and validated. The originalist judiciary perceives piton — a degraded interpretive task maintained through inertia. The analytical observer at civilizational scope risks perceiving mountain — the reading presented as inherent to constitutional language — but the structural data (beneficiaries identified, extraction mechanism clear, readings alternative) reveals this as a false summit. The perspectival gaps indicate that the militia-conditioned reading is not a natural discovery but an institutional choice with distributional consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is computed from power level, time horizon, exit options, and beneficiary/victim status. Gun owners (powerless + trapped + victim) experience maximum d and maximum f(d) — the constraint extracts from them structurally. Organized coalitions (organized + constrained + victim) experience moderate d — they have exit paths but at cost. State regulatory authority (institutional + arbitrage + beneficiary) experiences low d and negative f(d) — the constraint subsidizes state action. The firearms industry (institutional + constrained + mixed) experiences moderate d — they benefit from legitimacy but suffer regulatory fragmentation. The analytical observer (analytical + analytical) experiences d ≈ 0.72 and f(d) ≈ 1.15 — the standard analytical position, neither full beneficiary nor full victim but observing the extraction structure from outside. The piton classification (for the originalist judiciary) emerges not from high d but from high theater_ratio — the judicial engagement with founding intent is performative rather than functionally reconstructive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_grammatical_function,
    'Does the prefatory clause ''A well regulated Militia, being necessary to the security of a free State'' grammatically limit (condition) the operative clause ''the right of the people to keep and bear Arms, shall not be infringed'', or merely motivate it?',
    '18th-century grammatical analysis (historical usage of prefatory structures in founding-era texts; comparison with other constitutional clauses using similar structures); examination of founding-era militia law and practice to determine if ''militia'' referred exclusively to state-organized forces or included armed citizenry; linguistic intent recovery via historical documents, Federalist Papers, anti-Federalist responses, state convention debates, founding-era statutory definitions of militia.',
    'If prefatory clause genuinely CONDITIONS operative clause: state regulatory authority is presumed legitimate (militia-conditioned reading correct). If prefatory clause merely MOTIVATES: individual right to bear arms is more robust (individual-right reading gains strength). If grammatical function is irreducibly ambiguous across founding-era usage: both readings coexist and neither can claim sole textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_grammatical_function, empirical, 'Does the prefatory clause grammatically condition or merely motivate the operative clause?').

omega_variable(
    militia_definition_scope_historical,
    'In founding-era usage, did ''militia'' refer exclusively to state-organized military forces (select militia, formal state regiments) or did it include all able-bodied male citizens capable of bearing arms (unorganized militia)?',
    'Examination of founding-era state militia laws, militia statutes, military organization documents, militia musters, uniform codes, and constitutional convention debates. Cross-reference with Federalist Papers (Hamilton, Madison essays on military organization), Anti-Federalist responses, and state constitutions predating 1791. Comparison with British militia tradition and colonial practice.',
    'If militia = state-organized military only: prefatory clause clearly limits operative clause to state-militia context (militia-conditioned reading strengthened). If militia = all armed citizens: prefatory clause becomes inclusive rather than limiting (individual-right reading strengthened). If definition varies across founding-era usage: ambiguity permits both readings to claim textual support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_definition_scope_historical, empirical, 'Historical definition of militia: state-organized vs. unorganized armed citizenry').

omega_variable(
    constitutional_amendment_vs_doctrine_drift,
    'Is the current Second Amendment jurisprudence (post-DC v. Heller 2008, post-McDonald v. Chicago 2010) an interpretation of the constitutional text or a substantive amendment of constitutional meaning through doctrine?',
    'Historical tracking of Supreme Court doctrine evolution: United States v. Miller (1939, militia-conditioned), DC v. Heller (2008, individual-right recognition), McDonald v. Chicago (2010, incorporation). Comparative analysis: does Heller''s originalist methodology recover historical meaning, or does it apply modern originalist standards retroactively to founding-era sources? Assessment of whether militia-conditioned reading was the settled constitutional rule from 1791-2008 and Heller represents a reversal.',
    'If doctrine drift = reinterpretation of consistent text: Second Amendment meaning was always ambiguous and Heller made explicit what was implicit. If doctrine drift = substantive constitutional amendment: the militia-conditioned reading was the binding law for 217 years and Heller represents a revolution in constitutional meaning. This affects whether the militia-conditioned reading is ''the'' reading or ''a'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_vs_doctrine_drift, conceptual, 'Whether post-2008 jurisprudence is interpretation or substantive constitutional amendment').

omega_variable(
    modern_militia_concept_applicability,
    'If militia-conditioned reading is correct, what constitutes a ''well-regulated Militia'' in modern context (professional standing military, National Guard, state-organized militia, unorganized militia reserve, armed citizenry)?',
    'Statutory analysis of modern militia organization (federal law definitions in 10 U.S.C. § 246, state militia statutes, National Guard authority structures). Assessment of whether modern context permits the same understanding as founding era. Evaluation of whether ''well-regulated'' in militia-conditioned reading means state-approved regulation or merely ''effective functioning.''',
    'If militia-conditioned reading requires state approval for arms bearing: suppression is high and state regulatory authority is maximal. If militia-conditioned reading permits unorganized militia interpretation: individual arms-bearing may be recognized within militia frame. If modern militia concept is incoherent: militia-conditioned reading becomes anachronistic and loses applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_militia_concept_applicability, conceptual, 'What modern institutional form satisfies ''well-regulated Militia'' requirement').

omega_variable(
    rational_basis_scrutiny_scope,
    'Under militia-conditioned reading''s presumption that state regulation is legitimate, how broad is the rational-basis standard? Can any conceivable rational basis justify any regulation (extreme deference), or are there meaningful limits (intermediate scrutiny disguised as rational-basis)?',
    'Review of Supreme Court rational-basis doctrine in Second Amendment cases. Examination of which state regulations have survived rational-basis scrutiny and which have been struck down. Comparison with rational-basis application in other constitutional domains (economic regulation, rational-basis review of laws). Assessment of whether courts actually apply rational-basis (nearly insurmountable barrier for challenging regulation) or unstated intermediate scrutiny.',
    'If rational-basis = true extreme deference: any state regulation survives, suppression is maximal, and the militia-conditioned reading empowers comprehensive state regulation. If rational-basis = coded intermediate scrutiny: some regulations can be challenged, suppression is moderate, and individual rights retain some protection. This determines whether the tangled-rope classification holds or should shift to rope (pure state benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_basis_scrutiny_scope, empirical, 'Actual strictness of rational-basis scrutiny applied to Second Amendment claims').

omega_variable(
    kernel_reading_stability,
    'Is the militia-conditioned reading a stable constitutional interpretation, or is it inherently unstable under pressure from the individual-right and insurrectionist readings?',
    'Tracking of Supreme Court composition and potential doctrine shift. Assessment of whether Heller''s individual-right logic, once established in precedent, can be re-confined to militia context by future courts, or whether doctrine ratchets. Examination of lower court resistance to militia-conditioned interpretation and appeals court splitting. Evaluation of whether public opinion and political mobilization (gun-rights advocacy vs. public-safety advocacy) create permanent drift away from militia-conditioned stability.',
    'If reading is stable: militia-conditioned constraint persists as structuring principle. If reading is unstable: the constraint will drift toward individual-right reading or split into parallel regimes (conservative courts recognizing individual right; progressive courts re-emphasizing militia condition). This affects the long-term classification and measurement trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, preference, 'Long-term stability of militia-conditioned reading as binding constitutional interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa2a_militia_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sa2a_militia_tr_t50, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(sa2a_militia_tr_t100, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(sa2a_militia_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sa2a_militia_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(sa2a_militia_be_t100, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, firearms_licensing_regime).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, gun_violence_reduction_policy).

% DUAL FORMULATION NOTE:
% The Second Amendment boundary is a contested kernel admitting three structurally distinct readings with different ε values. The militia-conditioned reading (this story, ε=0.52) treats the prefatory clause as limiting and confers regulatory authority on states. The individual-right reading (separate story, ε≈0.35) treats the prefatory clause as merely motivating and recognizes an individual right subject to narrow regulation. The insurrectionist reading (separate story, ε≈0.58) recognizes an individual right specifically protecting armed resistance. All three readings operate on the same text but instantiate different constraints because they produce different beneficiary/victim structures, different suppression mechanisms, and different regulatory outcomes. The extractiveness values differ because the readings construe the constitutional authority differently. Network links trace how courts' adoption of one reading affects the plausibility and institutional support for other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
