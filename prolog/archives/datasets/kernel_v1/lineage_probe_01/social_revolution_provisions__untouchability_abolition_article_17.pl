% ============================================================================
% CONSTRAINT STORY: social_revolution_provisions__untouchability_abolition_article_17
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_revolution_provisions__untouchability_abolition_article_17, []).

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
 *   constraint_id: social_revolution_provisions__untouchability_abolition_article_17
 *   human_readable: Constitutional Abolition of Untouchability (Article 17, Indian Constitution)
 *   domain: legal/constitutional/social_revolution
 *
 * SUMMARY:
 *   Article 17 of the Indian Constitution, adopted in 1950, abolished
 *   untouchability as a constitutional offense enforceable against private
 *   actors directly. This was a radical legal innovation: it converted a
 *   millennia-old ritual practice into a criminalized violation, naming
 *   Dalits explicitly as constitutional subjects entitled to freedom from
 *   exclusion. The abolition is simultaneously a genuine social revolution
 *   (the formal legal order declares untouchability impermissible) and a
 *   structural paradox (the constraint requires continuous enforcement
 *   against entrenched practice). This story instantiates the 'untouchability
 *   abolition' reading of the contested kernel 'social revolution
 *   provisions,' which can also be read through the lenses of personal law
 *   compromise (caste-based personal laws permitted to continue in family
 *   domains) and reservation architecture (affirmative action as the
 *   fulfillment rather than exception of equality). This reading emphasizes
 *   the horizontal enforcement dimension (binds private conduct directly) and
 *   models the constraint as tangled_rope: genuine coordination (abolition
 *   principle) layered with asymmetric extraction (enforcement burden falls
 *   on Dalit mobilization; beneficiary is formalized as constitutional
 *   subject while material capacity for self-protection remains limited).
 *
 * KEY AGENTS:
 *   - Dalits as constitutional subjects: Primary beneficiary (powerless/trapped) — formally named in Article 17 as entitled to freedom from untouchability, yet enforcement depends on state capacity and Dalit self-mobilization.
 *   - Ritual enforcers of caste exclusion: Victim group (moderate/constrained) — criminalized for practicing untouchability; experience suppression (0.72) through legal liability, yet benefit from coordination of caste structure around non-exclusionary mechanisms.
 *   - Constitutional state apparatus: Institutional beneficiary (institutional/arbitrage) — gains legitimacy and coordination efficiency through the abolition principle; experiences no extraction through this mechanism.
 *   - Dalit social movements and advocacy organizations: Organized agents (organized/constrained) — weaponize Article 17 through litigation and enforcement mobilization; benefit from legal platform while bearing continuous enforcement labor costs.
 *   - Judiciary: Institutional performer (institutional/arbitrage) — applies Article 17 through largely performative ritual; maintains legitimacy while material enforcement remains underfunded (piton perspective).
 *   - Analytical observer: Civilizational viewer (analytical/analytical) — risks reading abolition as natural law discovery while the structural data reveals false summit (beneficiaries named, enforcement gaps evident).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_revolution_provisions__untouchability_abolition_article_17, 0.38).
domain_priors:suppression_score(social_revolution_provisions__untouchability_abolition_article_17, 0.72).
domain_priors:theater_ratio(social_revolution_provisions__untouchability_abolition_article_17, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_revolution_provisions__untouchability_abolition_article_17, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_revolution_provisions__untouchability_abolition_article_17, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_revolution_provisions__untouchability_abolition_article_17, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_revolution_provisions__untouchability_abolition_article_17, tangled_rope).
narrative_ontology:human_readable(social_revolution_provisions__untouchability_abolition_article_17, "Constitutional Abolition of Untouchability (Article 17, Indian Constitution)").
narrative_ontology:topic_domain(social_revolution_provisions__untouchability_abolition_article_17, "legal/constitutional/social_revolution").

domain_priors:requires_active_enforcement(social_revolution_provisions__untouchability_abolition_article_17).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_revolution_provisions__untouchability_abolition_article_17, '893463f1-a845-4d25-859f-7664a4259c12').
narrative_ontology:cs_kernel_codification('893463f1-a845-4d25-859f-7664a4259c12', formalized).
narrative_ontology:cs_authority_grounding('893463f1-a845-4d25-859f-7664a4259c12', lineage).
narrative_ontology:cs_interpretation_layer_present('893463f1-a845-4d25-859f-7664a4259c12').
narrative_ontology:cs_reading_relation('893463f1-a845-4d25-859f-7664a4259c12', social_revolution_provisions__personal_law_compromise, coexists_with).
narrative_ontology:cs_reading_relation('893463f1-a845-4d25-859f-7664a4259c12', social_revolution_provisions__reservation_architecture, influences).
narrative_ontology:cs_axiom('893463f1-a845-4d25-859f-7664a4259c12', foundational, untouchability_absolutely_prohibited).
narrative_ontology:cs_axiom_status(untouchability_absolutely_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('893463f1-a845-4d25-859f-7664a4259c12', untouchability_absolutely_prohibited, deontological).
narrative_ontology:cs_axiom('893463f1-a845-4d25-859f-7664a4259c12', foundational, state_enforcement_capacity_required).
narrative_ontology:cs_axiom_status(state_enforcement_capacity_required, holdable).
narrative_ontology:cs_axiom_grounding('893463f1-a845-4d25-859f-7664a4259c12', state_enforcement_capacity_required, instrumental).
narrative_ontology:cs_reference_frame('893463f1-a845-4d25-859f-7664a4259c12', absolute_personhood_framework).
narrative_ontology:cs_drift_state('893463f1-a845-4d25-859f-7664a4259c12', contemporary_enforcement_landscape, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('893463f1-a845-4d25-859f-7664a4259c12', '').
narrative_ontology:cs_kernel_id(social_revolution_provisions__untouchability_abolition_article_17, social_revolution_provisions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_revolution_provisions__untouchability_abolition_article_17, dalits_as_constitutional_subjects).
narrative_ontology:constraint_victim(social_revolution_provisions__untouchability_abolition_article_17, ritual_enforcers_of_caste_exclusion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DALIT SUBJECT (SNARE) — Formally named as beneficiary of constitutional abolition, but the practice persists through enforcement mechanisms that criminalize its agents rather than eliminate its material conditions. The Dalit subject is trapped by the gap between textual guarantee and enforcement capacity — constitutionally protected but materially blocked from claiming protection without state enforcement that remains structurally underfunded. High suppression reflects the persistence of the practice despite prohibition.
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RITUAL ENFORCER / CASTE AUTHORITY (TANGLED ROPE) — Constrained by criminal liability for practicing untouchability (suppression=0.72), yet the constraint also coordinates caste authority's own reproduction: by criminalizing the practice explicitly, Article 17 also delineates it as separable from other aspects of community membership, enabling selective abandonment while maintaining caste hierarchy through other mechanisms (purity, marriage, occupation). The enforcer experiences mixed extraction (liability) and benefit (coordination of caste structure around non-exclusionary mechanisms).
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL STATE APPARATUS (ROPE) — Experiences Article 17 as a pure coordination mechanism: it resolves the foundational social contract by naming a category of persons (Dalits) as constitutional subjects entitled to abolition of a practice. The state benefits from the legitimacy of this commitment and from the reduced transaction costs of a unified legal order without ritual exclusion. No extraction runs from the state toward other parties through this mechanism — the coordination is genuine.
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DALIT SOCIAL MOVEMENTS / ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents that weaponize Article 17 through litigation, enforcement mobilization, and public naming. Experience both coordination (Article 17 provides a legal platform for collective action that did not exist before) and asymmetric extraction (the constraint requires continuous enforcement labor from Dalit organizations while the constitutional guarantee remains formally passive, passing enforcement costs downward). Benefit from the mechanism through legal leverage; constrained by the need to maintain perpetual enforcement campaigns.
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUDICIARY (PITON) — Courts apply Article 17 through a largely performative ritual: cases are heard, verdicts are issued, but enforcement and material change remain underfunded and depend on police action that itself reproduces caste hierarchies. The judicial mechanism persists through institutional inertia and legitimacy maintenance — the courts perform their role in the social revolution while material conditions remain structurally stable. Theater ratio is low (0.35) because the constitutional text is unambiguous and the judicial interpretation straightforward; piton classification derives from the disconnection between judgment and enforcement.
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational perspective, the abolition of untouchability can be read as the discovery of an immutable principle: ritual exclusion from shared humanity is logically incompatible with human dignity (natural law of the person). This perspective sees Article 17 as the constitutional articulation of an unchangeable truth about personhood, not a contingent institutional arrangement. However, the structural data reveals this as a false summit: identified beneficiaries (Dalits as constitutional subjects) and enforcement gaps indicate this is a constructed constraint (contingent institutional creation) naturalizing its own legitimacy.
constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_revolution_provisions__untouchability_abolition_article_17_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_revolution_provisions__untouchability_abolition_article_17, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_revolution_provisions__untouchability_abolition_article_17, TR),
    TR >= 0.70.

:- end_tests(social_revolution_provisions__untouchability_abolition_article_17_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, declining over time from 0.72 pre-1950 to 0.38 contemporary. The trajectory reflects that untouchability practices have materially declined as enforcement strengthened and social movements mobilized, reducing the extractive gap between constitutional guarantee and material practice. However, extractiveness remains non-zero because enforcement still depends substantially on Dalit self-mobilization rather than state capacity alone. The initial pre-1950 value of 0.72 represents the pure extraction of caste-based untouchability (maximal asymmetry between ritual enforcers and excluded persons). Suppression (0.72): High and rising from 0.05 to 0.72 over the interval. This unusual trajectory (inverse to extractiveness) reflects that Article 17's criminalization increased the *enforcement burden* on the constraint system itself. Pre-abolition, untouchability required minimal enforcement because it was normalized; post-abolition, the practice persists but must be suppressed through criminal law, police action, and litigation. The rising suppression reflects the constraint's active enforcement dimension. Theater (0.35): Low. Article 17's text is unambiguous, and judicial interpretation is straightforward — there is little room for performative ambiguity in the doctrine. Piton classification derives not from high theater but from disconnection between judgment and enforcement (courts issue verdicts; material change depends on underfunded state apparatus).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal across this constraint family. Dalits see entrapment (snare): formally protected but materially blocked, dependent on state enforcement that remains structurally underfunded. Ritual enforcers see mixed constraint (tangled_rope): criminalized for practicing untouchability, but the criminalization also coordinates caste structure's adaptation around non-exclusionary mechanisms. The state sees pure coordination (rope): unified legal order, reduced transaction costs, legitimacy gain, no extraction. Dalit movements see weaponizable coordination (tangled_rope): legal platform for collective action, but with perpetual enforcement burden. The judiciary sees degraded performance (piton): applying clear doctrine while material change depends on enforcement capacity elsewhere. The analytical observer risks seeing natural law (mountain) while the structural data reveals false summit (named beneficiaries, enforcement gaps, constructed constraint). The constraint exemplifies how a single constitutional declaration can be read as six different types depending on observer position and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values differ sharply across perspectives. Dalits as beneficiaries appear in terms of constitutional naming, yet their structural exit options remain trapped (high d, high f(d) → high experienced χ). Ritual enforcers of caste exclusion are victims of criminalization (high d), yet they benefit from coordination of caste structure (low d simultaneously). The split reflects that this constraint is tangled_rope at the moderate power level: mixed extraction and coordination. The institutional state apparatus has arbitrage exit (low d → negative χ), experiencing net benefit. The analytical observer risks d=0.72 (canonical for analytical power) on the false summit, seeing an immutable truth when the structural data reveals contingency. No directionality overrides are needed; the derivation chain correctly captures the perspectival heterogeneity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading apparatus. The core mandatrophy question is: 'Is abolishing untouchability a discovered law of human dignity or a constructed institutional mechanism?' The false summit omega (above) makes this explicit. From the powerless Dalit perspective at biographical time, the constraint is snare (formal right without material capacity). From the analytical observer at civilizational time, it appears as mountain (natural law). These are not reconcilable into a single classification because they answer the same mandatrophy question differently. The reading_relations structure (below) shows that this reading coexists_with personal law compromise reading but influences reservation architecture reading. The mandatrophy is resolved by accepting that both are live interpretations: Article 17 genuinely abolishes untouchability as a constitutional offense (discovered law dimension) while that abolition requires continuous enforcement against entrenched practice (constructed constraint dimension). The tension is not a bug in the classification system; it is the structural reality the constraint captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_sufficiency,
    'Does state enforcement capacity exist to criminalize untouchability practice against entrenched caste authority, or does criminalization merely declare a principle while leaving enforcement to Dalit self-mobilization?',
    'Historical tracking of Article 17 prosecutions; resource allocation to enforcement agencies; longitudinal data on conviction rates and remedial outcomes; comparison with enforcement capacity for other constitutional prohibitions.',
    'If enforcement capacity high: tangled_rope classification confirmed (genuine coordination + modest extraction through enforcement burden). If enforcement capacity low: snare classification for powerless agents confirmed (constitutional guarantee without enforcement mechanism = entrapment in formal rights without material capacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_sufficiency, empirical, 'Whether state has capacity to enforce Article 17 criminalization').

omega_variable(
    ritual_substitution_and_structural_caste,
    'Can untouchability practice be abolished while caste hierarchy remains structurally intact, or is the practice constitutive of the entire caste system such that its abolition requires caste dissolution?',
    'Ethnographic and historical analysis of post-Article 17 caste practice: persistence of purity/pollution logic through non-exclusionary mechanisms; marriage patterns; occupational segregation; land control; ritual status; wealth accumulation. Comparison with pre-abolition practice frequency to assess substitution vs. elimination.',
    'If substitution possible: Article 17 is genuine coordination mechanism (caste structure can persist without the practice). If constitutive: Article 17 is false summit, masking deeper structural extraction that persists through alternative mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_substitution_and_structural_caste, empirical, 'Whether untouchability practice is separable from caste hierarchy or constitutive of it').

omega_variable(
    dalit_constitutional_subject_formation,
    'Does naming Dalits as constitutional subjects in Article 17 create new legal standing and political identity, or does it formalize existing stigmatized category without transforming power relations?',
    'Analysis of litigation patterns post-Article 17; emergence of Dalit rights movements and their framing; shifts in political representation; comparative study of constitutional naming in other contexts (e.g., indigenous peoples, religious minorities); longitudinal tracking of Dalit political organization and collective power.',
    'If genuine new subject formation: Article 17 is structurally transformative (creates new agency). If formalization of stigmatized category: Article 17 is false summit (appears to create subjects while consolidating surveillance/control of marginalized group).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dalit_constitutional_subject_formation, conceptual, 'Whether Article 17 creates new Dalit constitutional subject or formalizes existing stigmatization').

omega_variable(
    reading_foreclosure_test,
    'Does the Article 17 reading of absolute abolition foreclose the personal law compromise reading (which permits caste-based personal laws to continue), or can both readings coexist within the constitutional framework?',
    'Legal doctrine analysis: examination of how courts have handled conflicts between Article 17 abolition and personal law provisions (Hindu Marriage Act, Christian Personal Law, etc.); case law on whether caste-based personal law is compatible with untouchability abolition; whether courts have read the provisions as modifying each other or as operating in separate domains.',
    'If foreclosed: reading_relations should be ''forecloses'' (Article 17 logically requires personal law domains to exclude caste-based discrimination). If coexist: reading_relations should be ''coexists_with'' (courts permit both readings through domain separation). This determines whether the sibling reading is structurally impossible or merely contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether Article 17 abolition forecloses personal law compromise or permits coexistence').

omega_variable(
    false_summit_candidate,
    'Is Article 17''s abolition of untouchability a discovered natural law of human dignity, or a constructed constraint that benefits Dalits by transforming their formal legal status while potentially consolidating state control over caste categories?',
    'Comparative analysis of abolition vs. other social revolutions: does the constitutional declaration of personhood produce material equality, or does it create new forms of surveillance, bureaucratization, and category-based governance? Historical trajectories of rights declarations across contexts.',
    'If discovered law: mountain classification by analytical observer is genuine (untouchability is incompatible with dignity as a timeless truth). If constructed constraint: mountain is false summit, and the constraint is tangled_rope at civilizational scope (benefits Dalits through rights declaration while consolidating state apparatus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether Article 17 abolition is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_revolution_provisions__untouchability_abolition_article_17, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_1950, social_revolution_provisions__untouchability_abolition_article_17, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_1965, social_revolution_provisions__untouchability_abolition_article_17, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(extractiveness_pre_1950, social_revolution_provisions__untouchability_abolition_article_17, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(extractiveness_1955, social_revolution_provisions__untouchability_abolition_article_17, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(extractiveness_1965, social_revolution_provisions__untouchability_abolition_article_17, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(extractiveness_contemporary, social_revolution_provisions__untouchability_abolition_article_17, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppression_pre_1950, social_revolution_provisions__untouchability_abolition_article_17, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(suppression_1955, social_revolution_provisions__untouchability_abolition_article_17, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(suppression_1965, social_revolution_provisions__untouchability_abolition_article_17, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(suppression_contemporary, social_revolution_provisions__untouchability_abolition_article_17, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_revolution_provisions__untouchability_abolition_article_17, enforcement_mechanism).
narrative_ontology:affects_constraint(social_revolution_provisions__untouchability_abolition_article_17, social_revolution_provisions__personal_law_compromise).
narrative_ontology:affects_constraint(social_revolution_provisions__untouchability_abolition_article_17, social_revolution_provisions__reservation_architecture).

% DUAL FORMULATION NOTE:
% The social_revolution_provisions kernel contains three structurally distinct constraints corresponding to three readings: (1) untouchability_abolition_article_17 (ε=0.38, Tangled Rope) — horizontal abolition of practice, enforcement-intensive; (2) personal_law_compromise (ε ≈ 0.50, Tangled Rope) — permits caste-based personal law in family domains; (3) reservation_architecture (ε ≈ 0.45, Tangled Rope) — affirmative action as fulfillment of equality. Each reading implies different beneficiary/victim structures and different extractiveness values. The untouchability_abolition reading influences the other two: it establishes the constitutional prohibition that creates pressure on personal law domains (influences) and establishes the equality baseline that reservation architecture fulfills (influences). The three stories together model how a single constitutional kernel is interpreted through competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
