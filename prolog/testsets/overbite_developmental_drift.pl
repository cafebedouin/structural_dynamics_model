% ============================================================================
% CONSTRAINT STORY: overbite_developmental_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overbite_developmental_drift, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: overbite_developmental_drift
 *   human_readable: Overbite Developmental Drift via Fork Adoption
 *   domain: cultural_anthropology/technology_adoption/behavioral_evolution
 *
 * SUMMARY:
 *   The spread of fork adoption in medieval and early modern Europe provides
 *   a case study in how technology-mediated behavioral coordination can
 *   select for morphological change without conscious design or identifiable
 *   extraction. From the 11th century onward, the fork progressively replaced
 *   knife-and-hand techniques in elite European dining, spreading through
 *   social signaling and institutional enforcement (etiquette systems,
 *   religious dining practices, aristocratic table manners). This behavioral
 *   shift — using a small, pronged tool to manipulate food rather than
 *   cutting with a knife and bringing food to the mouth with hands or fingers
 *   — altered the mechanical demands on the jaw. Fork use requires finer
 *   motor control and less forceful biting, reducing lateral grinding and
 *   selecting for vertical bite closure. Over 5-6 generations, populations in
 *   fork-adopting regions developed measurably different bite configurations:
 *   reduced edge-to-edge bite (more overbite), more anterior tooth dominance,
 *   and different molar wear patterns compared to populations maintaining
 *   knife-and-hand techniques. The constraint demonstrates coordination
 *   without obvious extraction (fork use benefits everyone in the adopting
 *   community through social coherence) but with latent institutional
 *   consequences: modern orthodontia treats the resulting overbite as a
 *   pathology requiring correction, and the original technology choice — fork
 *   adoption as cultural coordination — is completely invisible to the
 *   clinical gaze. This case tests whether constraints can be naturalized and
 *   enforced through institutional inertia (piton status) when no
 *   identifiable agent benefits from the original choice or its invisibility.
 *
 * KEY AGENTS:
 *   - Fork-Adopting Populations: Primary beneficiary (institutional/arbitrage) — gain social coherence, status signaling, and table ritual coordination from fork use; no extraction occurs at the population level
 *   - Individual Children in Fork Cultures: Structural target (powerless/trapped) — inherit fork-use norms and experience resultant bite development; no conscious extraction or coercion
 *   - Etiquette Enforcement Systems: Secondary institutional actor (institutional/arbitrage) — reinforce fork-use norms through shame, status assignment, and social ritual; benefit from coordination clarity but don't extract from individuals
 *   - Fork Manufacturing Industries: Tertiary institutional actor (institutional/arbitrage) — benefit from demand for standardized cutlery; unclear whether they actively promoted adoption or rode existing waves
 *   - Modern Orthodontic System: Institutional actor (institutional/arbitrage) — manages the invisible consequences of fork adoption as a pathology; theater is high because the underlying technology choice is absent from clinical framing
 *   - Alternative Eating Technology Communities: Organized agents (organized/mobile) — see fork dominance as a temporary lock-in with visible exit pathways; represent potential constraint sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent cultural choice as an immutable biomechanical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overbite_developmental_drift, 0.18).
domain_priors:suppression_score(overbite_developmental_drift, 0.35).
domain_priors:theater_ratio(overbite_developmental_drift, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overbite_developmental_drift, extractiveness, 0.18).
narrative_ontology:constraint_metric(overbite_developmental_drift, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(overbite_developmental_drift, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overbite_developmental_drift, rope).
narrative_ontology:human_readable(overbite_developmental_drift, "Overbite Developmental Drift via Fork Adoption").
narrative_ontology:topic_domain(overbite_developmental_drift, "cultural_anthropology/technology_adoption/behavioral_evolution").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overbite_developmental_drift, fork_adopting_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL EXPERIENCING DRIFT (ROPE) — A child born into a fork-adopting culture inherits both the technology and the behavioral patterns it selects for. The individual is structurally mobile (could theoretically use knife-and-hands techniques) but functionally locked into fork use by social coordination norms and the unavailability of alternative utensil traditions in their community. No extraction occurs at the individual level — this is pure coordination. The overbite development itself is incidental to the coordination function.
constraint_indexing:constraint_classification(overbite_developmental_drift, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COMMUNITY MAINTAINING FORK NORMS (ROPE) — The community benefits from and enforces fork-use coordination. Shared cutlery practices reduce social friction, enable common table rituals, and align individuals' behavioral repertoires. The constraint is pure coordination with minimal coercion — exit requires relocating to a non-fork culture or investing in counter-socialization against the dominant norm. Suppression exists (social shame for 'eating like a peasant' with fingers) but is mild. No beneficiary captures surplus value; the benefit is distributed as social coherence.
constraint_indexing:constraint_classification(overbite_developmental_drift, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a biomechanical standpoint, fork use selects for specific jaw mechanics: reduced lateral grinding (forks cut food fine rather than requiring heavy molar crushing), favoring vertical bite closure and anterior tooth dominance. This can be framed as a 'natural' consequence of the technology — the mouth simply adapts to the mechanical demands of fork eating. However, this naturalizes what is actually a path-dependent cultural choice. The perspective risks false-summit classification: the constraint is presented as an immutable biomechanical law ('overbite is how modern humans naturally developed') when it is actually a contingent cultural adoption pattern with identifiable beneficiaries (fork-adopting populations gain social coordination; non-fork populations maintain alternative bite configurations).
constraint_indexing:constraint_classification(overbite_developmental_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MODERN DENTAL SYSTEM (PITON) — Contemporary orthodontics treats overbite as a 'natural' deviation requiring correction via braces and extraction. The dental system has institutionalized the management of a culturally-contingent bite pattern as a pathology. Theater is high (the orthodontic ritual of diagnosis, treatment planning, and correction is performative relative to the actual biomechanical requirement) because the underlying constraint — fork use — is invisible to the clinical gaze. Orthodontists see individual mouth geometry, not the technology-behavior-morphology pipeline. The system persists because institutional investment in correction is substantial; the underlying coordination (fork use) is treated as given. This is a piton: a former coordination mechanism (fork use as social coherence) whose primary function is now obscured, maintained through institutional inertia and theatrical correction practices.
constraint_indexing:constraint_classification(overbite_developmental_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE EATING TECHNOLOGIES MOVEMENT (SCAFFOLD) — Small but organized movements (zero-waste communities, traditional food cultures, bio-hacking subcultures) are experimenting with non-fork eating tools and bite-friendly food preparation. These movements see fork dominance as a temporary coordination lock-in with a sunset clause: as food processing technology diversifies and cross-cultural eating practices resurface, the fork-use selection pressure could relax. The constraint is temporary coordination with a visible exit pathway. Beneficiaries of fork-dominant culture face lower costs if eating norms diversify (unlike snare victims who face total entrapment). Theater is low in these communities because eating is means rather than ritual.
constraint_indexing:constraint_classification(overbite_developmental_drift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FORK-MANUFACTURING AND ETIQUETTE SYSTEMS (ROPE) — The global institutional ecosystem of fork production, restaurant service standards, etiquette norms, and tableware manufacturing benefits from fork dominance. These institutions maintain the coordination through standardization (place settings, formal dining conventions, status signaling). Exit is available at institutional level (alternative cutlery systems exist and have been adopted historically) but is constrained by network effects and sunk capital. The institutions experience the constraint as coordination with significant infrastructure value — no extraction occurs at the institutional level, though downstream effects (overbite prevalence, orthodontic market demand) are externalized.
constraint_indexing:constraint_classification(overbite_developmental_drift, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overbite_developmental_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overbite_developmental_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overbite_developmental_drift, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(overbite_developmental_drift, TR),
    TR >= 0.70.

:- end_tests(overbite_developmental_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Fork adoption spreads through social coordination (status signaling, etiquette enforcement, ritual participation) with minimal direct extraction. Individuals benefit from participation in coordinated dining norms. The extractiveness increases over time as the fork-use selection pressure accumulates and alternative eating techniques atrophy. By time-point 500, extractiveness rises to 0.18 because the constraint becomes increasingly locked-in — the availability of alternatives (knife-and-hand techniques, traditional finger foods, non-Western utensils) decays as populations lose familiarity with these methods. This represents not extraction by an agent, but extraction through path-dependence: later generations cannot easily exit fork-use norms without acquiring counter-cultural skills their socialization never provided. Suppression (0.35): Moderate. Social shame ('eating like a peasant'), institutional enforcement (formal dining standards, religious conventions), and educational indoctrination (children learning fork-use as 'proper' behavior) create barriers to alternatives. However, suppression is not severe — knife-and-hand techniques remain technically available, and some populations maintain non-fork utensil traditions. The rise from 0.08 to 0.35 reflects increasing normalization: fork use becomes so ingrained that alternatives are not merely shameful but cognitively unavailable — children do not learn non-fork techniques, making exit effectively impossible without external knowledge. Theater ratio (0.42): Moderate. Fork-use as social ritual has higher theater content than pure functional eating. Etiquette enforcement emphasizes correct technique over efficiency. The rise from 0.15 to 0.42 reflects increasing institutional elaboration: fork-use becomes entangled with status display, ritual correctness, and aesthetic performance. The modern orthodontic system (perspective 4) operates entirely in theater — correcting overbite as pathology rather than addressing the technology choice that selected for it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the paradox of path-dependent naturalization without obvious extraction. The fork-adopting community sees pure coordination (Rope, perspective 2) — they benefit from shared eating norms. The individual child sees coordination (Rope, perspective 1) — they are socialized into normal behavior. The institutional systems see coordination with some performance function (Rope, perspective 6) — tableware manufacturers and etiquette enforcers maintain the system. The modern dental system sees pathology requiring correction (Piton, perspective 4) — orthodontia has institutionalized the management of a cultural contingency as a medical problem. Alternative communities see a temporary lock-in with an exit pathway (Scaffold, perspective 5) — eating-technology diversity could relax the selection pressure. The analytical observer risks seeing an immutable biomechanical law (Mountain, perspective 3, false summit candidate) — 'overbite is how humans naturally developed' naturalizes a contingent choice. The perspectival gap reveals that the constraint's invisibility is its defining feature: no agent designed it, no agent extracts obvious value from it, yet it becomes enforced through institutional inertia and naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the structural relationship between agent and constraint. Fork-adopting populations have low d (0.15-0.20): they benefit from coordination, have mobile exit options at the population level, and experience no extraction. Individual children have higher d (0.50-0.65): they are born into the system, experience behavioral constraints through socialization, and have limited exit options during development. The piton perspective (orthodontic system) has low d (0.10): the system benefits from the constraint's invisibility and would experience reduced demand if fork-use history were visible. The analytical observer has moderate-high d (0.65-0.75): from the civilizational perspective, the observer is embedded in the fork-dominated world and cannot easily see the original choice as contingent. The scaffold perspective has low d (0.20): organized alternatives have arbitrage potential — they can opt out and profit from positioning themselves as offering a different coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT IDENTITY TEST: This case tests whether the Deferential Realism framework can distinguish genuine coordination (Rope) from path-dependent naturalization (false-summit Mountain) when beneficiaries are distributed and institutional inertia has rendered the original choice invisible. Fork-use coordination exhibits zero mandate violation: the constraint has a genuine coordination function (aligned eating norms, social coherence, table ritual), low extraction, low suppression initially (rising over time as alternatives decay). The mandatrophy is resolved by recognizing that the constraint is Rope at the origin (time 0) but approaches Piton status at time 500 as theater increases and the original choice becomes invisible. The modern response (institutional management through orthodontia) risks false-summit reclassification because it treats the downstream morphological effect as pathology rather than tracing the constraint to its source (technology adoption). The mandatrophy is resolved by declaring the constraint as Rope with piton-stage institutional consequences rather than as a Mountain natural law. This classification prevents the error of naturalizing a technology-mediated behavioral pattern as biomechanical destiny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_mechanism_ambiguity,
    'Is the observed overbite increase driven by biomechanical selection pressure from fork use, or is it a side effect of caloric abundance and earlier eruption times in post-Medieval European populations?',
    'Comparative skeletal analysis: populations using forks with high caloric intake vs. populations using forks with restricted intake; populations with high caloric intake but non-fork utensils. Controlled for eruption timing, nutritional status, and dental wear patterns.',
    'If fork-driven: constraint classification is valid as rope (technology-mediated coordination selecting for bite pattern). If caloric-driven: the constraint is nutritional allocation (different constraint family), and fork adoption is correlated but not causal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_mechanism_ambiguity, empirical, 'Whether overbite increase is fork-driven or nutrition-driven').

omega_variable(
    reversibility_and_lock_in,
    'If a population abandoned fork use after 500 years, would overbite selection reverse in subsequent generations, or is the morphological change locked in and irreversible?',
    'Longitudinal population genetics; analysis of populations that have adopted or abandoned fork use in recent centuries; epigenetic markers of developmental constraint.',
    'If reversible: the constraint is a soft coordination lock-in (rope/scaffold). If irreversible: the constraint has accrued irreversible consequences and approaches mountain-class or snare-class status for those locked into overbite morphology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_and_lock_in, empirical, 'Whether overbite selection is reversible or locked-in').

omega_variable(
    beneficiary_identification_gap,
    'Do identifiable beneficiaries exist (fork manufacturers, etiquette enforcers, orthodontists) who extract value from fork-use coordination, or is this pure coordination with no structural extraction?',
    'Historical analysis of fork manufacturing industries, tableware trade, and orthodontic market emergence; identification of whether these industries actively promoted fork use or merely rode adoption waves.',
    'If identifiable extractive beneficiaries: constraint may be tangled_rope rather than rope. If pure coordination: the constraint demonstrates coordination without extraction, and false-summit risk comes only from naturalizing the contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_gap, empirical, 'Whether fork-use coordination has identifiable extractive beneficiaries').

omega_variable(
    path_naturalization_without_agency,
    'Can a constraint become naturalized (treated as immutable) and enforced through institutional inertia (piton status) even when no identifiable agent designed or benefits from the naturalization?',
    'Comparative analysis of overbite naturalization with other technology-mediated constraints (e.g., alphabet adoption altering visual processing, keyboard adoption altering hand posture, smartphone grip altering thumb morphology). Examine whether institutional response (orthodontia, ergonomic standards, therapeutic interventions) tracks agent awareness of the original technology choice.',
    'If naturalization can occur without agency: the framework''s assumption that all constraints have identifiable beneficiaries is challenged. Piton classification becomes applicable to pure coordination that has simply become invisible. Mountain false-summit detection must account for constraints that are genuinely immutable BECAUSE they are locked-in cultural choices, not because they are laws of nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_naturalization_without_agency, conceptual, 'Whether path-dependent naturalization requires agent design or can emerge through institutional drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overbite_developmental_drift, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(overbite_tr_t0, overbite_developmental_drift, theater_ratio, 0, 0.15).
narrative_ontology:measurement(overbite_tr_t250, overbite_developmental_drift, theater_ratio, 250, 0.28).
narrative_ontology:measurement(overbite_tr_t500, overbite_developmental_drift, theater_ratio, 500, 0.42).

% Extraction over time
narrative_ontology:measurement(overbite_be_t0, overbite_developmental_drift, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(overbite_be_t250, overbite_developmental_drift, base_extractiveness, 250, 0.08).
narrative_ontology:measurement(overbite_be_t500, overbite_developmental_drift, base_extractiveness, 500, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(overbite_su_t0, overbite_developmental_drift, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(overbite_su_t250, overbite_developmental_drift, suppression_requirement, 250, 0.2).
narrative_ontology:measurement(overbite_su_t500, overbite_developmental_drift, suppression_requirement, 500, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overbite_developmental_drift, resource_allocation).
narrative_ontology:affects_constraint(overbite_developmental_drift, orthodontic_treatment_normalization).
narrative_ontology:affects_constraint(overbite_developmental_drift, cutlery_standardization_lock_in).

% DUAL FORMULATION NOTE:
% The overbite developmental drift is downstream of fork adoption as cultural technology. The fork adoption itself is a separate constraint (coordination mechanism selecting for etiquette norms); the morphological consequence (overbite selection) is the constraint analyzed here. These are linked via the behavioral-to-morphological pipeline: fork-use behavior → altered jaw mechanics → developmental selection → institutional invisibility. Each constraint in the family has distinct ε and base properties. Network edges indicate structural influence: the orthodontic system's piton status depends on the underlying fork-use coordination remaining invisible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overbite_developmental_drift, powerless, 0.65).
constraint_indexing:directionality_override(overbite_developmental_drift, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
