% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous Override Reading: Federal Coercion and Doctrinal Suspension
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The Exogenous Override reading frames the 1890 Manifesto as federal
 *   coercion forcing institutional capitulation on plural marriage practice
 *   while maintaining the doctrinal claim that marriage doctrine itself
 *   remains unchanged. From this reading, the constraint exhibits high
 *   extractiveness (ε = 0.68): the federal government benefits from policy
 *   compliance without directly violating religious liberty (the institution
 *   can continue teaching the doctrine); the LDS membership and institutional
 *   leadership bear costs through forced practice abandonment, cognitive
 *   dissonance, and suppression of institutional autonomy. The core claim of
 *   this reading is that an external political authority (federal government)
 *   has unilaterally altered institutional practice while the institution
 *   maintains a face-saving theoretical claim that doctrine persists
 *   unchanged. This reading coexists with (at minimum) the endogenous
 *   reinterpretation reading, which frames the Manifesto as legitimate
 *   institutional reinterpretation of the marriage covenant in light of new
 *   understanding, and the hybrid pragmatic reading, which treats the
 *   Manifesto as both response to coercion AND genuine doctrinal development.
 *   The exogenous override reading is the most skeptical of institutional
 *   legitimacy claims, attributing the practice change to political power
 *   rather than theological insight.
 *
 * KEY AGENTS:
 *   - Federal Government: Primary beneficiary (institutional/arbitrage) — achieves polygamy cessation through institutional compliance; frames enforcement as legitimate state interest in marriage law (not religious persecution). Experiences the constraint as coordination: rule of law uniformly applied.
 *   - LDS Membership: Primary victim (powerless/trapped) — forced to abandon plural marriage practice while maintaining identity within institution. Bears cost of doctrinal-practice gap: theology teaches one thing, enforcement demands another. Cannot exit without abandoning religious identity, family bonds, cultural belonging.
 *   - Institutional Leadership: Secondary victim (institutional/constrained) — faces material coercion (property seizure, criminal prosecution, institutional dissolution threat). Must extract compliance from membership to maintain institutional survival. Experiences double extraction: federal pressure from above, member pressure from below.
 *   - Doctrinal System: Degraded authority (institutional/arbitrage) — doctrine persists as teaching but loses functional authority to govern practice. Authority claim ('doctrine unchanged') becomes performative cover for practical abandonment.
 *   - Reform Movement/Dissident Factions: Organized resistance (organized/constrained) — members who accept the change as legitimate doctrinal development or who resist as institutional betrayal. Some benefit from reform (legal protections, institutional stability); others bear cost (excommunication, ideological rupture).
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing political coercion as inevitable state authority, treating federal power as a natural law constraint on religious groups.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.68).
domain_priors:suppression_score(exogenous_override_reading, 0.72).
domain_priors:theater_ratio(exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, snare).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous Override Reading: Federal Coercion and Doctrinal Suspension").
narrative_ontology:topic_domain(exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, '7dcd727e-08e2-422d-af10-585f8ab0681e').
narrative_ontology:cs_created_at('7dcd727e-08e2-422d-af10-585f8ab0681e', '').
narrative_ontology:cs_kernel_codification('7dcd727e-08e2-422d-af10-585f8ab0681e', fixed_text).
narrative_ontology:cs_authority_grounding('7dcd727e-08e2-422d-af10-585f8ab0681e', extraction).
narrative_ontology:cs_interpretation_layer_present('7dcd727e-08e2-422d-af10-585f8ab0681e').
narrative_ontology:cs_kernel_id(exogenous_override_reading, marriage_commitment_legitimacy).
narrative_ontology:cs_reading_relation('7dcd727e-08e2-422d-af10-585f8ab0681e', endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('7dcd727e-08e2-422d-af10-585f8ab0681e', hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('7dcd727e-08e2-422d-af10-585f8ab0681e', foundational, federal_coercion_determines_practice).
narrative_ontology:cs_axiom_status(federal_coercion_determines_practice, holdable).
narrative_ontology:cs_axiom_grounding('7dcd727e-08e2-422d-af10-585f8ab0681e', federal_coercion_determines_practice, empirically_contingent).
narrative_ontology:cs_axiom('7dcd727e-08e2-422d-af10-585f8ab0681e', foundational, doctrinal_claim_unchanged_is_performance).
narrative_ontology:cs_axiom_status(doctrinal_claim_unchanged_is_performance, holdable).
narrative_ontology:cs_axiom_grounding('7dcd727e-08e2-422d-af10-585f8ab0681e', doctrinal_claim_unchanged_is_performance, deontological).
narrative_ontology:cs_reference_frame('7dcd727e-08e2-422d-af10-585f8ab0681e', eternal_covenant_authority).
narrative_ontology:cs_drift_state('7dcd727e-08e2-422d-af10-585f8ab0681e', post_manifesto_crisis, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(exogenous_override_reading, doctrinal_integrity).
narrative_ontology:constraint_victim(exogenous_override_reading, institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LDS MEMBERSHIP (SNARE) — Trapped within institutional identity; cannot exit without abandoning religious identity, family bonds, and social position. Forced to accept doctrinal suspension as external imposition. Bears cost of cognitive dissonance between theological claim (doctrine unchanging) and material practice (compliance with secular law). No exit options; maximum suppression from federal authority + internal institutional enforcement.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL LEADERSHIP (SNARE) — Leadership faces material coercion (loss of property, incarceration of officials, institutional dissolution threat). Constrained by institutional survival logic: must maintain both doctrinal legitimacy claim and actual compliance. Extraction flow is unidirectional — federal government extracts compliance; institution extracts cost from members. Leadership experiences high suppression and extraction simultaneously.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — Net beneficiary. Achieves policy objective (polygamy cessation) while avoiding direct religious persecution charge by allowing institution to maintain doctrinal claim. Federal authority experiences the constraint as coordination: enforcement of law is legitimate state function, and institutional compliance solves the plural marriage problem without federal assumption of religious authority. Low experienced extraction because state has power and options.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINAL SYSTEM (PITON) — Doctrine itself becomes performative: the claim that doctrine is 'unchanging' is maintained while practice is unilaterally altered by external coercion. The doctrinal authority structure persists (sermons about eternal marriage continue) but has lost functional control over membership behavior. Theater ratio is moderate (0.55) because institutional performance work maintains doctrinal claim while acknowledging practical constraint. Doctrine persists through inertia and identity maintenance, not functional authority.
constraint_indexing:constraint_classification(exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM MOVEMENT / DISSIDENTS (TANGLED ROPE) — Organized members resisting the coercive reading see genuine doctrinal coordination value (eternal marriage covenant) alongside state extraction (forced abandonment). Some members benefit from reform (women gain legal protections, institutional stability improves), creating hybrid extraction-coordination structure. Exit is costly (excommunication, family rupture) but possible through migration or organized dissent. Experiences both constraint and agency.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, state authority over marriage law is treated as a natural constraint on religious practice: all states regulate marriage, therefore religious groups must comply. This perspective naturalizes the political conflict as inevitable structural necessity rather than contingent coercion. However, the beneficiary/victim data contradicts the mountain classification — the engine will flag this as a false summit revealing naturalization of political power imbalance.
constraint_indexing:constraint_classification(exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε = 0.68): High. The federal government extracts institutional compliance through threat of property seizure, criminal prosecution, and institutional dissolution — material coercion at the highest level. The institution cannot negotiate; it must choose between doctrinal commitment and institutional survival. The initial extractiveness (0.42) reflects pre-crisis negotiation attempts; the crisis point (t=3) marks the shift to federal ultimatum (Manifesto, property seizure, prosecution), raising extractiveness to 0.68 and maintaining it through the interval. The extraction flow is unidirectional: federal government to institution to membership. Suppression (σ = 0.72): High. Federal enforcement includes criminal prosecution of polygamists, property seizure, imprisonment of officials, and institutional dissolution threat. Internal institutional enforcement includes social pressure, excommunication threat, and identity-based coercion (choosing between religion and legal compliance). Membership has limited exit options: formal exit requires abandoning religious identity and family bonds (trapped); constrained migration was attempted but ultimately insufficient. Theater ratio (τ = 0.55): Moderate. The institutional performance work maintains the doctrinal claim ('doctrine is unchanged') while acknowledging practical constraint through the Manifesto frame ('suspended under duress'). The theater is not maximal (piton level) because the institution does not fully hide the coercion — the Manifesto explicitly names external pressure. But the theater exceeds pure extraction (snare level) because the institution invests significant work in reconciling the doctrinal claim with the practice change. This moderate theater reflects an institution in transition: moving from piton (doctrine fully performative) toward either snare (coerced compliance) or tangled rope (if doctrinal reinterpretation succeeds).
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading generates maximum perspectival divergence. Federal government classifies as Rope (coordination of law enforcement). Membership classifies as Snare (pure extraction, no exit). Leadership classifies as Snare or Tangled Rope depending on whether leaders recognize benefit from institutional survival or only cost from coercion. Doctrine itself classifies as Piton (persists through inertia, not functional authority). This reading's perspectival structure — beneficiary sees coordination, victims see extraction, institution degrades to theater — is diagnostic of exogenous coercion. The alternative readings (endogenous reinterpretation, hybrid pragmatic) would show different perspectival signatures: less gap between leadership and membership, less theater in the doctrinal claim, less snare classification for membership (identity lock instead of trap, or constrained instead of trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position and exit options. Federal government (institutional/arbitrage) has maximum options and power — derives d ≈ 0.05 (full beneficiary). LDS membership (powerless/trapped) has minimum options — derives d ≈ 0.95 (full target). Institutional leadership (institutional/constrained) experiences both extraction and survival incentives — derives d ≈ 0.60-0.70 (mixed target/beneficiary). The perspectival gap reflects this: federal government sees coordination (rope), membership sees extraction (snare), leadership sees mixed coercion-necessity (tangled rope or snare depending on identity frame). The false summit at the analytical observer level reveals that 'state authority over marriage law is natural' naturalizes the political power imbalance rather than explaining it. The exogenous override reading treats this naturalizing frame as a false summit precisely because it obscures the contingent coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the exogenous override reading is ONE perspective on a contested kernel. The kernel is 'marriage commitment legitimacy' — what makes a marriage doctrine legitimate? The exogenous override reading answers: institutional authority claims legitimacy, but external political power overrides it. The endogenous reinterpretation reading answers: institutional authority evolves its understanding through theological reflection. The hybrid pragmatic reading answers: both coercion and genuine theological development operate simultaneously. No single answer is correct — the authority structure itself is contested. The constraint story captures the exogenous override reading as a coherent structural claim: federal power extracts institutional compliance, the institution maintains theoretical doctrinal claim while practicing abandonment, and the membership bears the cost of the gap. This reading coexists with others because different factions within the institution (and outside it) genuinely hold different readings with different classification outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_suspension_permanence,
    'Is the doctrinal suspension permanent or genuinely temporary pending future political change?',
    'Historical tracking: if federal pressure eases or theocratic conditions reemerge, does institution resurrect the suspended doctrine? Institutional rhetoric analysis: do official statements treat suspension as permanent policy or provisional accommodation?',
    'If permanent: this is genuine doctrinal reinterpretation masked as suspension (forecloses exogenous override reading, collapses to endogenous_reinterpretation_reading). If temporary: the suspension is extractive coercion (confirms snare classification). The claim that ''doctrine is unchanged'' is empirically testable by observing what happens if external pressure ceases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_suspension_permanence, empirical, 'Whether doctrinal suspension is permanent or contingent on external coercion').

omega_variable(
    membership_cognitive_lock,
    'Do members genuinely believe the theoretical claim that doctrine is unchanged while practice is suspended? Or is this a collective performance that members recognize as incoherent?',
    'Ethnographic analysis of internal discourse: what do members say when not in official contexts? Do private testimonies acknowledge the gap, or do they report genuine cognitive integration? Survey instruments measuring perceived coherence of doctrinal claim.',
    'If members believe the claim: suppression mechanism is primarily institutional authority (maintains power through frame control). If members recognize incoherence but accept it: suppression mechanism is identity fusion + institutional coercion (trapped by identity lock rather than just institutional authority). Different impacts on stability and future classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_cognitive_lock, empirical, 'Whether members cognitively integrate or reject the doctrine/practice gap').

omega_variable(
    alternative_reading_stability,
    'Could the exogenous override reading and the endogenous reinterpretation reading both be operative simultaneously within the same institution, held by different factions?',
    'Organizational analysis: institutional documents and official pronouncements (treat as exogenous override frame); member discourse and theological reinterpretation work (treat as endogenous reinterpretation frame). Measure faction size and institutional power distribution between frames.',
    'If both readings coexist: the constraint should be reclassified as ''coexists_with'' rather than ''forecloses''. The institution itself is the contested site. If one frame achieves institutional dominance: relationship changes to ''influences'' (dominant frame shapes conditions for minority frame).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_stability, empirical, 'Whether exogenous override and endogenous reinterpretation readings coexist within institution').

omega_variable(
    kernel_identity_contingency,
    'Is the marriage commitment kernel itself contingent on the specific doctrinal form (plural marriage), or does the kernel describe a more abstract commitment (eternal covenant) that could accommodate the reformed practice?',
    'Theological analysis: does authoritative doctrine identify plural marriage as essential to the covenant, or is it one instantiation of a more fundamental commitment? Historical precedent: has the institution previously reframed fundamental commitments in response to external pressure?',
    'If kernel is form-contingent: exogenous override forecloses endogenous reinterpretation (incompatible commitments). If kernel is form-independent: the readings coexist (same kernel, different readings of how to instantiate it). This determines the logical structure of the reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_contingency, conceptual, 'Whether marriage covenant kernel is form-contingent or form-independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_tr_t0, exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exog_tr_t3, exogenous_override_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(exog_tr_t6, exogenous_override_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(exog_be_t0, exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exog_be_t3, exogenous_override_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(exog_be_t6, exogenous_override_reading, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage commitment legitimacy kernel decomposes into three readings with distinct ε values and perspectival signatures. This story is the exogenous override reading (high extraction, snare-dominant, federal coercion frame). Sibling stories capture endogenous reinterpretation (lower extraction, doctrinal development frame) and hybrid pragmatism (mixed extraction, both coercion and development). All three share the same kernel but disagree on whether the practice change is coercive imposition, legitimate reinterpretation, or both. Affects relationship: exogenous override reading, if dominant, forecloses or constrains endogenous reading's authority claim (coercion undermines legitimacy). Endogenous reading, if dominant, reinterprets away exogenous reading's core premise (coercion is reframed as opportunity for growth). Hybrid reading coexists with both (acknowledges both mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
