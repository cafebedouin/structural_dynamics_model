% ============================================================================
% CONSTRAINT STORY: regulatory_pathway_psychedelic_therapy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_pathway_psychedelic_therapy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_pathway_psychedelic_therapy
 *   human_readable: The Regulatory and Clinical Pathway for Novel Psychedelic Therapies
 *   domain: technological/political
 *
 * SUMMARY:
 *   The regulatory pathway for psychedelic therapy represents a hybrid
 *   coordination-extraction constraint where legitimate therapeutic oversight
 *   (FDA's coordination function) is coupled with significant extraction
 *   through capital barriers, timeline suppression, and monopoly gatekeeping.
 *   Patients with treatment-resistant depression or PTSD face both
 *   suppression (legal prohibition, access restriction) and extraction (high
 *   medication costs, delayed access during multi-year trials). Independent
 *   research teams face capital barriers ($100M+) that favor well-capitalized
 *   incumbent pharmaceutical companies. The constraint exhibits all
 *   manifestations of a tangled rope: genuine coordination function
 *   (establishing safety standards, preventing proliferation of unsafe
 *   compounds), asymmetric beneficiaries (pharmaceutical companies capture
 *   exclusive rents), asymmetric victims (patients, researchers, field
 *   innovation), and active enforcement (DEA scheduling, FDA trial
 *   requirements). The theater ratio is high (0.68) and increasing because
 *   regulatory approval rhetoric emphasizes safety and scientific rigor while
 *   the actual approval bar has become increasingly disconnected from
 *   comparative international evidence standards and from real-world
 *   therapeutic outcomes. Breakthrough therapy designations and
 *   decriminalization efforts represent scaffold-like reform mechanisms
 *   building toward sunset of the traditional regulatory monopoly.
 *
 * KEY AGENTS:
 *   - Treatment-Seeking Patients: Primary victims (powerless/trapped) — lack legal access, face suppression, bear costs of delayed innovation and high medication prices
 *   - Independent Biotech/Research Teams: Secondary victims (moderate/constrained) — face $100M+ capital barriers, 10+ year development timelines, institutional gatekeeping
 *   - Incumbent Pharmaceutical Companies: Primary beneficiaries (institutional/arbitrage) — capture exclusive market access, monopoly pricing, regulatory moat protection
 *   - FDA Regulatory Agency: Organized enforcer (organized/constrained) — derives legitimacy and resource justification from pharmaceutical gatekeeping; constrained by safety liability and political pressure for faster access
 *   - DEA Controlled Substance Scheduling: Institutional actor (institutional/arbitrage) — maintains symbolic prohibition; increasingly performative as state-level decriminalization creates alternative pathways
 *   - Breakthrough Therapy / Reform Coalition: Organized advocates (organized/constrained) — patient groups, reform-minded researchers, some FDA/political staff building accelerated pathways and decriminalization as alternatives to classical regulatory monopoly
 *   - Mental Health Innovation Field: Collective victim (powerless/trapped) — entire therapeutic domain experiences suppressed innovation and delayed access to potentially transformative compounds due to regulatory barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, 0.58).
domain_priors:suppression_score(regulatory_pathway_psychedelic_therapy, 0.72).
domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_pathway_psychedelic_therapy, tangled_rope).
narrative_ontology:human_readable(regulatory_pathway_psychedelic_therapy, "The Regulatory and Clinical Pathway for Novel Psychedelic Therapies").
narrative_ontology:topic_domain(regulatory_pathway_psychedelic_therapy, "technological/political").

domain_priors:requires_active_enforcement(regulatory_pathway_psychedelic_therapy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, incumbent_pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, regulatory_agencies).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, treatment_seeking_patients).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, psychedelic_research_teams).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, mental_health_field_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREATMENT-SEEKING PATIENT (SNARE) — Faces severe depression or treatment-resistant PTSD with limited alternatives. Cannot exit the regulatory pathway; experimental compounds are illegal regardless of personal medical judgment. Bears full cost of suppression (restricted access) while extraction flows upward to pharmaceutical companies and regulatory gatekeepers. No alternatives available, no voice in the process, no exit option.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT RESEARCH TEAM (SNARE) — Small biotech firms or academic researchers pursuing psychedelic therapy must navigate FDA Phase I-III trial requirements costing $100M+ over 10+ years. Exit options are severely constrained: can attempt international development (regulatory arbitrage in some contexts) but this fragments the market. Cannot commercialize in the US without FDA approval. Faces suppression through capital barriers, regulatory complexity, and institutional gatekeeping. High experienced extraction.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PHARMACEUTICAL COMPANY (ROPE) — Large pharma benefits from regulatory pathway's coordination function: FDA approval creates legal monopoly, protects against generics during patent exclusivity, and establishes dosing/labeling standards that reduce liability. Extraction runs toward this agent — they capture rents from exclusivity while using the pathway to coordinate market access. High exit optionality (can pursue other indications, have capital for 10+ year trials, can lobby for extension). Experiences the constraint as enabling coordination and value capture.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (FDA) (TANGLED ROPE) — Organized institution enforcing the pathway, but also constrained by it. FDA derives organizational legitimacy and resource justification from managing pharmaceutical approval (coordination function) but also extracts by setting approval bar high enough that only well-capitalized firms can succeed (extraction asymmetry). Benefits from gatekeeping role; constrained by political pressure for faster approvals, safety litigation risk, and limited staff. Experiences both coordination (pharmaceutical safety standards) and asymmetric extraction (capital barriers to entry).
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BREAKTHROUGH THERAPY DESIGNATION (BTD) REFORM PATHWAY (SCAFFOLD) — Organized advocates (patient groups, reform-minded researchers, some FDA staff) see the regulatory pathway as a temporary problem with a visible sunset: accelerated approval programs, breakthrough designations, expanded access pathways, and decriminalization efforts are creating alternatives that bypass traditional Phase III gatekeeping. From this view, the classical regulatory monopoly is degrading in real time. Theater ratio high (performative safety arguments) but declining as alternative verification methods (real-world outcomes data, adaptive trials) mature. Exit path visible — reform has begun.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONTROLLED SUBSTANCE SCHEDULING (PITON) — The DEA's Schedule I classification of psychedelics persists largely through institutional inertia despite decades of evidence that compounds like psilocybin have low abuse potential and potential therapeutic value. The scheduling system functions theatrically: it maintains symbolic prohibition while the actual therapeutic demand is met through decriminalization in some jurisdictions and compassionate use programs. The primary function (preventing abuse) has atrophied; the constraint persists due to bureaucratic and political inertia, not because it works. High theater, low functionality, institutional maintenance of outdated rules.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, any novel therapeutic compound must prove safety and efficacy before deployment at population scale — this is presented as an immutable constraint on medical practice. Rigorous trial requirements are framed as inherent to responsible pharmacology. However, this naturalizes what is actually a contingent institutional choice: regulatory rigor is culture and policy-dependent, not a law of nature. Other jurisdictions have approved psychedelic therapies with different evidence thresholds. The mountain classification reveals false naturalization.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_pathway_psychedelic_therapy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, TR),
    TR >= 0.70.

:- end_tests(regulatory_pathway_psychedelic_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regulatory pathway extracts value through multiple mechanisms: (1) Capital barriers ($100M+ for FDA trials) that exclude most independent researchers and small teams; (2) Timeline suppression (10+ years from discovery to approval) that delays patient access and accrues monopoly benefits; (3) Exclusive market access during patent period that enables monopoly pricing; (4) Gatekeeping that converts therapeutic discovery into captured value for well-capitalized incumbents. However, extractiveness is not as high as pure snare (0.66+) because some of the suppression genuinely serves coordination (safety standards, efficacy verification). Suppression (0.72): Very high. The constraint combines legal prohibition (DEA scheduling), capital barriers (FDA trial costs), knowledge barriers (regulatory expertise), and institutional gatekeeping. Patients face near-total suppression (cannot legally access); independent researchers face severe suppression (capital and timeline barriers); even incumbents face moderate suppression (regulatory complexity, liability risk). Theater ratio (0.68): High and increasing. FDA approval rhetoric emphasizes rigorous safety science, but the approval bar has become increasingly disconnected from international comparators and from real-world outcome data. Breakthrough therapy designations introduce performative acceleration (appear to reduce timelines while maintaining gatekeeping). Controlled substance scheduling maintains symbolic prohibition despite declining evidence base. The theater is increasing because alternative verification methods (decriminalization, real-world outcomes, compassionate access) are producing equivalent or better outcomes while regulatory pathway approval remains slow, suggesting that the regulatory theater exceeds functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional mechanism (FDA regulatory approval) can be perceived as lifesaving coordination (from incumbent pharma view) or as extractive gatekeeping (from patient/researcher view). The incumbent pharmaceutical company sees the pathway as enabling coordination — FDA approval creates legal certainty, establishes dosing standards, protects against liability, and grants monopoly rents as just reward for bearing trial costs and innovation risk. This view is partly accurate; the coordination function is real. The treatment-seeking patient sees pure extraction and suppression — the regulatory pathway is the mechanism that keeps them from accessing a potentially life-changing therapy, with no voice in the decision. The breakthrough therapy coalition sees a temporary problem with a visible exit: decriminalization, adaptive trials, and real-world outcomes data are building alternative pathways that will eventually make the classical regulatory monopoly obsolete. The FDA itself experiences the pathway as tangled — it provides necessary gatekeeping (coordination) but also creates the suppression that makes it appear extractive. The controlled substance scheduling system appears as piton — a degraded ritual persisting through bureaucratic inertia rather than because it prevents abuse. The analytical observer risks seeing regulatory rigor as an immutable law of medical practice, naturalizing what is actually a contingent institutional choice that varies across jurisdictions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) and experienced effective extraction (χ) is derived from their structural position: beneficiary status, power level, and exit options. Treatment-seeking patients have d ≈ 0.95 (full victims, trapped) producing high f(d) ≈ 1.42 and high χ — they experience maximum extraction because they have no alternatives and no voice. Independent researchers have d ≈ 0.75 (victims, constrained) producing f(d) ≈ 1.15 and moderate-high χ — they can exit (international development) but at significant cost. Incumbent pharmaceutical companies have d ≈ 0.10 (beneficiaries, arbitrage) producing f(d) ≈ -0.05 and negative χ — they benefit from the pathway and have exit options (other indications, other markets). The FDA derives d ≈ 0.45 (constrained by political and liability risk, partly victim of their own gatekeeping) producing f(d) ≈ 0.45 and moderate χ. The breakthrough therapy coalition has d ≈ 0.35 (constrained but with visible exit path) producing f(d) ≈ 0.25 and low-moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that regulatory capture is not binary (pure coordination vs pure extraction) but rather a hybrid state where both functions operate simultaneously with asymmetric distribution. The FDA's legitimate coordination function (establishing safety standards, ensuring efficacy verification) is coupled with asymmetric extraction (capital barriers that benefit incumbents, timeline suppression that accrues monopoly rents). This is not a failure of regulation but a structural feature of regulatory design — the mechanisms that enable coordination (comprehensive testing, expert review, exclusive approval) simultaneously enable gatekeeping and extraction. The constraint satisfies both mandatrophy gates: (1) It exhibits genuine coordination function — the pathway does establish safety standards, prevent unsafe compounds from reaching market, and coordinate complex multi-trial processes that individual actors could not manage alone; (2) It exhibits asymmetric extraction — beneficiaries (pharmaceutical companies, regulatory agencies) capture positive rents while victims (patients, researchers, innovation field) bear suppression costs. The tangled_rope classification is correct because both functions are structural, not incidental. The theater ratio is high (0.68) because the coordination rhetoric (safety, rigor, science) has become decoupled from actual comparative evidence — alternative pathways (decriminalization, real-world outcomes) are producing equivalent safety and efficacy with lower theater. This suggests that the regulatory theater exceeds functional necessity and has shifted toward performing gatekeeping rather than enabling coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_evidence_bar,
    'What level of clinical evidence suffices to demonstrate psychedelic therapy safety and efficacy — Phase III RCTs (traditional), real-world outcomes data, adaptive trials, or other methodologies?',
    'Comparative analysis of regulatory standards across jurisdictions; meta-analysis of outcomes data from therapies approved via different pathways; validation of alternative trial designs against gold-standard evidence',
    'If alternative evidence standards prove equivalent: FDA bar is unnecessary suppression (extraction tightens). If Phase III requirement is necessary: current suppression is justified coordination. Determines whether regulatory pathway classifies as pure extraction or coordination-extraction hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_evidence_bar, empirical, 'Evidentiary sufficiency threshold for psychedelic therapy approval').

omega_variable(
    capital_barrier_intentionality,
    'Is the $100M+ cost of FDA trials an intentional gatekeeping mechanism or an incidental byproduct of comprehensive safety evaluation?',
    'Comparative cost analysis of FDA trials vs international regulatory pathways for equivalent compounds; historical analysis of FDA cost inflation over time; structural analysis of which cost components are safety-justified vs which serve as barriers to entry',
    'If intentional gatekeeping: extraction mechanism is clear (beneficiaries with capital captured the regulator). If incidental byproduct: suppression is unjustly high but not deliberately extractive — classification shifts from snare toward scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_barrier_intentionality, empirical, 'Whether regulatory costs function as intentional barriers to entry').

omega_variable(
    decriminalization_substitution,
    'Do decriminalization and compassionate access pathways constitute genuine alternatives to FDA regulation or merely shadow access for privileged populations?',
    'Longitudinal tracking of patient outcomes in jurisdictions with decriminalized access vs FDA-approved therapies; equity analysis of who gains access through each pathway; assessment of whether shadow access reduces pressure for formal regulatory reform',
    'If genuine alternatives: exit options for patients improve (constrained → mobile), classification shifts from snare toward tangled_rope. If shadow access only: trapped agents remain trapped, alternative pathways become pitons (theatrical alternatives without real function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decriminalization_substitution, empirical, 'Whether decriminalization provides genuine therapeutic access alternatives').

omega_variable(
    monopoly_rent_extraction,
    'What proportion of pharmaceutical profits from psychedelic therapies derives from breakthrough therapy value versus from FDA-granted market exclusivity and gatekeeping?',
    'Pricing analysis of approved psychedelic therapies in regulated vs non-regulated markets; comparison of profit margins to R&D costs and replication costs in alternative pathways; historical analysis of patent exclusivity periods and generic competition timelines',
    'If exclusivity rents are dominant: extraction mechanism confirmed (pharmaceutical company benefits are contingent on regulatory monopoly, not on innovation value). If breakthrough value is dominant: extraction is lower than assessed, constraint classifies as more coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_rent_extraction, empirical, 'Proportion of pharmaceutical profits from monopoly versus breakthrough innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_pathway_psychedelic_therapy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regpsy_tr_t0, regulatory_pathway_psychedelic_therapy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(regpsy_tr_t5, regulatory_pathway_psychedelic_therapy, theater_ratio, 5, 0.62).
narrative_ontology:measurement(regpsy_tr_t10, regulatory_pathway_psychedelic_therapy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(regpsy_be_t0, regulatory_pathway_psychedelic_therapy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(regpsy_be_t5, regulatory_pathway_psychedelic_therapy, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(regpsy_be_t10, regulatory_pathway_psychedelic_therapy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_pathway_psychedelic_therapy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_pathway_psychedelic_therapy, 0.45).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, psilocybin_therapeutic_efficacy).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, dmt_safety_profile_establishment).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, mental_health_treatment_access).

% DUAL FORMULATION NOTE:
% The regulatory pathway is downstream of specific therapeutic efficacy claims (psilocybin efficacy for depression, DMT safety for psychiatric use) but represents a distinct structural constraint. Upstream constraints have their own extractiveness values reflecting empirical status of therapeutic claims; the regulatory pathway has its own extractiveness reflecting institutional gatekeeping and capital barriers to market access. Decomposition: regulatory_pathway_psychedelic_therapy (this story, ε=0.58) operationalizes the institutional mechanisms that control access to psilocybin_therapeutic_efficacy (ε=0.08, mountain-candidate) and dmt_safety_profile_establishment (ε=0.22, rope-candidate). The pathway constraint is more extractive than the underlying scientific claims because institutional gatekeeping adds a second layer of suppression beyond empirical uncertainty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_pathway_psychedelic_therapy, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
