% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textualist Restriction on Analogical Reasoning
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Hanbali jurisprudential method enforces a textualist restriction on
 *   analogical reasoning, prioritizing the hadith corpus (narrated
 *   precedents) over the reasoned opinions of jurists. This constraint
 *   operates as a commitment system: Ahmad ibn Hanbal (d. 241 AH) and his
 *   successors grounded their methodological kernel in the principle that
 *   legal reasoning must remain tethered to explicit textual foundation
 *   rather than delegating to interpretive authority. The reading produces a
 *   particular legal structure: novel cases without direct textual precedent
 *   face a narrow pathway to legal permissibility, while scholars of hadith
 *   transmission gain epistemic authority. This constraint exhibits the
 *   six-type perspectival gap: commercial innovators facing novel cases
 *   experience it as pure extraction (Snare); merchants with established
 *   practices experience mixed coordination and extraction (Tangled Rope);
 *   hadith scholars experience it as coordination that stabilizes textual
 *   authority (Rope); reform movements experience it as a temporary
 *   constraint being eroded through maslaha reasoning (Scaffold);
 *   institutional courts experience it as degraded theater where textual
 *   formalism masks interpretive work (Piton); and analytical observers risk
 *   mistaking a contested methodological choice for a natural law of legal
 *   reasoning (Mountain, false summit). The measurement trajectory shows
 *   rising extractiveness and theater over five centuries: early Hanbali
 *   textualism (t=0, ε=0.35) was genuinely restrictive but had lower theater
 *   because jurists actually operated within textual bounds. Modern Hanbali
 *   jurisprudence (t=500, ε=0.52) achieves flexibility through interpretive
 *   workarounds (maslaha, istihsan, necessity doctrines) that maintain a
 *   textualist facade while enabling adaptation, raising the theater ratio
 *   substantially. The constraint is one reading of the
 *   jurisprudential_method_kernel; sibling readings (Hanafi, Maliki, Shafi'i)
 *   instantiate different distributions of authority between hadith scholars
 *   and jurist opinion, with different extraction profiles.
 *
 * KEY AGENTS:
 *   - Ahmad ibn Hanbal and Hanbali Institutional Hierarchy: Authority figure instantiating the textualist reading; benefits from institutional legitimacy grounded in textual foundation
 *   - Hadith Transmission Scholars: Primary beneficiaries (institutional/arbitrage) — elevated epistemic status through method's prioritization of hadith corpus
 *   - Commercial Innovators and Merchants Seeking Novel Cases: Primary victims (powerless/trapped; moderate/constrained) — face restriction on analogical reasoning for new practices
 *   - Jurists and Legal Reasoners: Secondary victims (institutional/constrained) — deprioritized in favor of hadith scholars; expertise-based reasoning suppressed
 *   - Reform Movements (19th–20th centuries): Organized agents (organized/constrained) — developing maslaha and other mechanisms that erode textualist restriction while maintaining appearance of faithfulness
 *   - Institutional Courts and Legal Authorities: Institutional actors maintaining the performative constraint (institutional/arbitrage) — publicly textualist while employing interpretive flexibility
 *   - Hanafi, Maliki, Shafi'i Schools: External alternatives (institutional/arbitrage) — represent competing readings of the jurisprudential kernel with different extraction profiles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.52).
domain_priors:suppression_score(hanbali_reading, 0.68).
domain_priors:theater_ratio(hanbali_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Jurisprudential Method: Textualist Restriction on Analogical Reasoning").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(hanbali_reading, fixed_text).
narrative_ontology:cs_authority_grounding(hanbali_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(hanbali_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, textual_literalists).
narrative_ontology:constraint_beneficiary(hanbali_reading, orthodox_institutional_authority).
narrative_ontology:constraint_victim(hanbali_reading, commercial_innovators).
narrative_ontology:constraint_victim(hanbali_reading, novel_case_applicants).
narrative_ontology:constraint_victim(hanbali_reading, legal_flexibility_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMERCIAL INNOVATOR (SNARE) — Faces a novel business practice (new contracts, trade mechanisms, financial instruments) with no explicit textual precedent in hadith. Cannot analogically reason from established cases because Hanbali method restricts analogy to cases with textual foundation. Trapped: the innovator cannot exit the jurisdiction's legal requirement, cannot access analogical reasoning mechanism, and cannot present arguments from jurist opinion (which the method deprioritizes). Full extraction — the method denies flexibility while maintaining the appearance of legal reasoning.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECONDARY MERCHANT/CRAFTSPERSON (TANGLED ROPE) — Benefits from the method's clarity and predictability: textual restriction eliminates arbitrary judicial discretion and protects established practices that have textual foundation. But also bears extraction: new techniques, partnerships, or market conditions requiring legal innovation face barriers. Constrained exit — can migrate to another legal school's jurisdiction, but at high social and business cost. Mixed coordination (textual stability) and extraction (innovation suppression).
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HADITH TRANSMISSION SCHOLARS & INSTITUTIONAL AUTHORITY (ROPE) — Primary beneficiary. The method prioritizes hadith corpus over jurist opinion, elevating transmission scholars' epistemic status and institutional authority. Jurists must justify reasoning through textual foundation rather than expertise-based analogy. Benefits from coordination function: the method stabilizes legal meaning by anchoring it to textual tradition rather than delegating to interpreters. Arbitrage exit — scholars can exit to other schools if their status declines, but institutional inertia favors remaining. Net beneficiary.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENT / METHODOLOGICAL INNOVATORS (SCAFFOLD) — Organized agents recognizing that the restriction on analogical reasoning has become brittle: real commercial complexity outpaces the ability to find explicit textual precedents. Modern reformers (late 19th–20th century) have developed maslaha (public interest) reasoning and other mechanisms that circumvent strict textualism while maintaining a facade of textual compliance. These mechanisms represent a sunset pathway: they slowly erode the restriction's force by establishing interpretive techniques that permit flexibility within a textualist frame. Constrained exit — reformers cannot publicly abandon the method (institutional legitimacy requires appearing faithful) but can operationalize alternatives.
constraint_indexing:constraint_classification(hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL LEGAL SYSTEM (PITON) — The formal method persists through institutional inertia and theater. Courts and legal authorities maintain the appearance of strict textualism while actually engaging in substantial interpretive work (through maslaha, istihsan, and other mechanisms) to reach pragmatic outcomes. The theater ratio is moderate-high: the public-facing method is textualist restriction; the actual mechanism is flexible interpretation. The method is degraded — it no longer performs its original function (preventing arbitrary judicial discretion) because institutional workarounds have hollowed it out. Yet it persists because abandoning the textual frame would damage institutional legitimacy.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical view, some restriction on analogical reasoning appears immutable: legal systems cannot function without limits on interpretation, and anchoring interpretation to text is a natural constraint on arbitrary authority. The textual method appears as a logical necessity rather than a contingent institutional choice. However, this perspective risks naturalizing what is actually a contested reading: other jurisprudential schools (Hanafi, Maliki, Shafi'i) demonstrate that non-Hanbali approaches sustain legal order without the same textualist restriction. The mountain classification is a false summit — the engine will identify it as a naturalization of a particular methodological choice.
constraint_indexing:constraint_classification(hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting the genuine restriction imposed on analogical reasoning. The method limits legal flexibility for novel cases, forcing innovators into a narrow textual search rather than permitting reasoned extension from established principles. However, the extraction is not maximal because institutional workarounds (maslaha reasoning) have emerged over time, gradually eroding the restriction's force. The measurement trajectory shows rising ε from 0.35 to 0.52, capturing this accumulation of extraction as the textual corpus became insufficient for real-world complexity. Suppression (0.68): High. Multiple barriers prevent exit: institutional legitimacy attached to the Hanbali school, social identity fusion with the community, cost of switching legal frameworks, and the theological claim that textualism is religiously required rather than methodologically contingent. Suppression reflects both structural barriers (institutional barriers to school-switching) and internalized barriers (identity fusion with the Hanbali interpretation). Theater ratio (0.55): Moderate-high. Early Hanbali practice (t=0) had low theater because jurists actually adhered to the textualist restriction (ε and theater were aligned). Modern practice (t=500) shows substantial theater: courts apply maslaha, necessity doctrines, and other interpretive mechanisms that functionally allow the flexibility that textualist restriction nominally forbids. The theater indicates institutional decay — the method's original function (constraining arbitrary discretion through textual foundation) has been bypassed, yet the formal textualist frame persists for legitimacy. The tangled_rope classification reflects that the method does perform a genuine coordination function (stabilizing legal meaning through textual grounding, preventing arbitrary judicial discretion) while simultaneously extracting from innovators who lack textual precedent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a full six-type perspectival range. The most extreme gap is between the hadith scholar (Rope: experiences the method as coordination stabilizing textual authority) and the commercial innovator (Snare: experiences pure extraction, with no self-correction mechanism). The moderate merchant (Tangled Rope) splits the difference — they benefit from the method's clarity and predictability for established practices but bear extraction costs for innovations. The reform movements (Scaffold) see the restriction as temporary, eroding through maslaha reasoning over generations. The institutional court system (Piton) sees its own formal textualism as performative theater masking actual interpretive flexibility. The analytical observer (Mountain) risks naturalizing this particular methodological choice as a logical necessity of legal interpretation, when in fact other jurisprudential schools demonstrate that legal order can be maintained without the same textualist restriction. This perspectival gap is diagnostic: it reveals that the 'natural law' framing (the textualist claim that this method follows logically from Islamic legal principles) is actually a contested institutional choice. The false summit classification for the analytical perspective is not a criticism but a structural signal: the constraint is one reading of a kernel, and alternative readings exist with different extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   See logic_rationale for detailed directionality derivation. Beneficiaries hold institutional positions and have exit options through school-switching (arbitrage exit), deriving low d and experiencing the method as coordination (rope). Victims lack textual precedent and face social/institutional costs of jurisdictional exit (trapped or constrained exit), deriving high d and experiencing the method as extraction (snare or tangled_rope). The piton classification derives from the theater gate rather than from high experienced extraction — the method's original function has been eroded by institutional workarounds, leaving formal textualism as a legitimacy ritual.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contested_kernel_reading,
    'Is the Hanbali textualist restriction a universal principle of sound jurisprudence, or one methodological choice among legitimate alternatives within Islamic law?',
    'Comparative analysis of legal outcomes under different jurisprudential schools; examination of whether Hanafi, Maliki, and Shafi''i methods produce inferior justice or stability; historical documentation of how each school handled novel cases',
    'If universal principle: mountain classification correct, extraction reading is misframing natural law. If contingent choice: false summit confirmed, tangled_rope and snare readings are correct, and the ''naturalness'' is institutional theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_reading, conceptual, 'Whether Hanbali textualism is a universal principle or a contested methodological reading').

omega_variable(
    textual_precedent_sufficiency,
    'Does the corpus of Qur''anic verses and hadith genuinely contain sufficient explicit precedent for the range of novel commercial and legal cases that arise, or does the claim of textual foundation mask extensive jurist interpretation?',
    'Case-by-case analysis of how Hanbali courts actually rule on novel cases; documentation of whether rulings map to explicit textual precedent or to inferred analogical chains; comparison with Hanafi reasoning chains for the same cases',
    'If sufficient: extraction model is wrong, and the method genuinely restricts unnecessary flexibility. If masked interpretation: theater ratio is higher than measured, and the method conceals interpretive discretion behind textualist framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_precedent_sufficiency, empirical, 'Whether textual corpus sufficiently covers novel cases or masked interpretation dominates').

omega_variable(
    maslaha_reform_erosion,
    'Does the maslaha (public interest) doctrine and other reform-era innovations actually constitute a sunset mechanism for textualist restriction, or do they merely add a performative layer of flexibility while the core restriction persists?',
    'Longitudinal analysis of how frequently maslaha reasoning is deployed; correlation with commercial innovation rates and legal permissibility patterns over time; comparison of innovation capacity under reform-era jurisprudence vs strict early textualism',
    'If genuine sunset: scaffold perspective is correct, and the restriction is dissolving. If performative layer: the restriction persists in force, and reformers'' innovations are theater masking continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_reform_erosion, empirical, 'Whether maslaha and reform innovations constitute a genuine sunset or performative layer').

omega_variable(
    orthodox_authority_capture,
    'Does prioritizing hadith transmission scholars and deprioritizing jurist opinion serve the legitimate goal of preventing arbitrary judicial discretion, or does it primarily benefit institutional authorities who consolidate power through textual gatekeeping?',
    'Historical analysis of who controlled hadith authentication and textual interpretation; examination of whether non-gatekeeping alternatives (Hanafi jurist-centered reasoning) produced worse outcomes or merely different power distributions',
    'If legitimate function: the extraction reading mischaracterizes a genuine coordination mechanism as extraction. If authority capture: beneficiary structure is correct, and the method''s primary function is consolidating institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_authority_capture, conceptual, 'Whether textual prioritization serves legitimate constraint or enables institutional authority capture').

omega_variable(
    jurisdictional_choice_vs_constraint,
    'Can a merchant or innovator realistically exit the Hanbali jurisdiction and adopt a Hanafi or Maliki school''s interpretation, or does institutional, social, and identity factors make exit functionally impossible despite formal legal feasibility?',
    'Ethnographic documentation of why merchants remain within Hanbali jurisdiction despite choosing more permissive schools elsewhere; analysis of social costs, family identity, institutional barriers to switching legal schools',
    'If exit feasible: exit_options should be ''mobile'' rather than ''trapped'' for commercial actors; constraint would classify as less severe extraction. If exit blocked by social/institutional factors: identity_locked may be more accurate than trapped, indicating cognitive/institutional capture rather than material confinement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_choice_vs_constraint, empirical, 'Actual feasibility of exiting Hanbali jurisdiction despite formal alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_theater_t0, hanbali_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hanbali_theater_t250, hanbali_reading, theater_ratio, 250, 0.48).
narrative_ontology:measurement(hanbali_theater_t500, hanbali_reading, theater_ratio, 500, 0.55).

% Extraction over time
narrative_ontology:measurement(hanbali_extract_t0, hanbali_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hanbali_extract_t250, hanbali_reading, base_extractiveness, 250, 0.48).
narrative_ontology:measurement(hanbali_extract_t500, hanbali_reading, base_extractiveness, 500, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).
narrative_ontology:affects_constraint(hanbali_reading, islamic_legal_innovation_barrier).
narrative_ontology:affects_constraint(hanbali_reading, hadith_scholar_epistemic_authority).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one member of the jurisprudential_method_kernel constraint family. Each reading (Hanbali, Hanafi, Maliki, Shafi'i) represents a distinct structural settlement of the authority question within Islamic law. They are not competing measurements of one constraint but rather competing instantiations of a contested kernel. The Hanbali reading has higher base extractiveness (0.52) and higher suppression (0.68) than the Hanafi alternative, reflecting its stricter textualism. Each reading should be authored as a separate constraint story with its own ε, perspectives, and beneficiary/victim declarations. The network links document how methodological choice in one school affects legal innovation capacity and epistemic authority in the broader Islamic legal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
