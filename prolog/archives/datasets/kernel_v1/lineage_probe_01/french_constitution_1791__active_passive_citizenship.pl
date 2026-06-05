% ============================================================================
% CONSTRAINT STORY: french_constitution_1791__active_passive_citizenship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_constitution_1791__active_passive_citizenship, []).

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
 *   constraint_id: french_constitution_1791__active_passive_citizenship
 *   human_readable: Active/Passive Citizenship Split (1791 French Constitution)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The 1791 French Constitution instantiates one specific reading of the
 *   contested revolutionary kernel: the nature of citizenship and political
 *   voice in a post-monarchical order. This reading — active/passive
 *   citizenship split indexed to direct taxation — directly contradicts the
 *   Declaration of Rights' universalist rhetoric. The Declaration, prefixed
 *   to the Constitution itself, declares 'Men are born free and remain free
 *   and equal in rights,' yet the Constitution operationalizes citizenship as
 *   a property-qualified franchise, creating a permanent structural
 *   contradiction at the heart of the revolutionary project. Active citizens
 *   (those paying direct taxes above a specified threshold) monopolize the
 *   vote, the ballot, and access to elected office. Passive citizens (the
 *   propertyless, women, servants) retain abstract rights but no political
 *   voice. This is not merely a franchise restriction — it is a bifurcation
 *   of personhood itself. The constraint extracts political power from the
 *   propertyless majority while preserving the fiction of universal rights,
 *   creating a performative gap between declared principles and operative
 *   structure. The measurements show rising suppression (0.68 → 0.78) as
 *   enforcement mechanisms tighten and peasant unrest prompts centralization
 *   of authority, and rising extractiveness (0.52 → 0.65) as the propertied
 *   class consolidates control and the propertyless realize the franchise is
 *   permanently closed. The theater ratio rises (0.30 → 0.42) as the gap
 *   between Declaration rhetoric and constitutional practice becomes
 *   undeniable.
 *
 * KEY AGENTS:
 *   - Tax-Qualified Active Citizens: Institutional beneficiary (institutional/arbitrage) — typically bourgeoisie, landowners, merchants above the tax threshold; capture political voice and consolidate property protections
 *   - Passive Citizens and Propertyless Classes: Primary victim (powerless/trapped) — peasants, urban laborers, servants, anyone below the tax threshold; structurally suppressed from the franchise
 *   - Women: Systemic victim (powerless/trapped) — explicitly or implicitly excluded from citizenship entirely; bear legal dependency on male household heads
 *   - The Legislative Assembly: Institutional beneficiary (institutional/arbitrage) — consists of active citizens only; experiences the property qualification as guaranteeing a propertied, rational electorate
 *   - The King and Monarchy: Mixed beneficiary/victim (institutional/constrained) — retains executive coordination power; constrained by the Assembly's supremacy in legislation and taxation; benefits from a propertied legislature less likely to push radical antimonarchical measures
 *   - The Constituent Assembly and Constitutional Authority: Institutional actor (institutional/arbitrage) — architects of the compromise; made deliberate choice to qualify the franchise despite Declaration universalism
 *   - Sans-Culottes Clubs and Radical Jacobins: Organized agents (organized/mobile) — see the property qualification as a transitional mistake; articulate universal suffrage as the true realization of revolutionary principles
 *   - The Declaration of Rights Itself: Textual kernel (analytical/analytical) — creates the performative contradiction by asserting universal rights while the Constitution implements their denial
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_constitution_1791__active_passive_citizenship, 0.58).
domain_priors:suppression_score(french_constitution_1791__active_passive_citizenship, 0.72).
domain_priors:theater_ratio(french_constitution_1791__active_passive_citizenship, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_constitution_1791__active_passive_citizenship, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_constitution_1791__active_passive_citizenship, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(french_constitution_1791__active_passive_citizenship, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_constitution_1791__active_passive_citizenship, tangled_rope).
narrative_ontology:human_readable(french_constitution_1791__active_passive_citizenship, "Active/Passive Citizenship Split (1791 French Constitution)").
narrative_ontology:topic_domain(french_constitution_1791__active_passive_citizenship, "political/constitutional").

domain_priors:requires_active_enforcement(french_constitution_1791__active_passive_citizenship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(french_constitution_1791__active_passive_citizenship, '17f4d307-320e-44dc-8b7c-1cf015d19e09').
narrative_ontology:cs_kernel_codification('17f4d307-320e-44dc-8b7c-1cf015d19e09', formalized).
narrative_ontology:cs_authority_grounding('17f4d307-320e-44dc-8b7c-1cf015d19e09', extraction).
narrative_ontology:cs_interpretation_layer_present('17f4d307-320e-44dc-8b7c-1cf015d19e09').
narrative_ontology:cs_reading_relation('17f4d307-320e-44dc-8b7c-1cf015d19e09', french_constitution_1791__declaration_of_rights_1789, coexists_with).
narrative_ontology:cs_reading_relation('17f4d307-320e-44dc-8b7c-1cf015d19e09', french_constitution_1791__suspensive_veto_monarchy, influences).
narrative_ontology:cs_reading_relation('17f4d307-320e-44dc-8b7c-1cf015d19e09', french_constitution_1791__failure_and_succession, coexists_with).
narrative_ontology:cs_axiom('17f4d307-320e-44dc-8b7c-1cf015d19e09', foundational, property_as_rational_franchise_criterion).
narrative_ontology:cs_axiom_status(property_as_rational_franchise_criterion, overridden).
narrative_ontology:cs_axiom_grounding('17f4d307-320e-44dc-8b7c-1cf015d19e09', property_as_rational_franchise_criterion, deontological).
narrative_ontology:cs_axiom('17f4d307-320e-44dc-8b7c-1cf015d19e09', foundational, passive_citizenship_as_rights_protection).
narrative_ontology:cs_axiom_status(passive_citizenship_as_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('17f4d307-320e-44dc-8b7c-1cf015d19e09', passive_citizenship_as_rights_protection, conventional).
narrative_ontology:cs_reference_frame('17f4d307-320e-44dc-8b7c-1cf015d19e09', propertied_rational_governance).
narrative_ontology:cs_drift_state('17f4d307-320e-44dc-8b7c-1cf015d19e09', august_1792_insurrection, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('17f4d307-320e-44dc-8b7c-1cf015d19e09', '').
narrative_ontology:cs_kernel_id(french_constitution_1791__active_passive_citizenship, french_constitution_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_constitution_1791__active_passive_citizenship, tax_qualified_active_citizens).
narrative_ontology:constraint_victim(french_constitution_1791__active_passive_citizenship, passive_citizens).
narrative_ontology:constraint_victim(french_constitution_1791__active_passive_citizenship, propertyless_classes).
narrative_ontology:constraint_victim(french_constitution_1791__active_passive_citizenship, women).
narrative_ontology:constraint_victim(french_constitution_1791__active_passive_citizenship, servants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTYLESS CLASSES (SNARE) — Trapped by property qualification; declared as rights-bearing citizens in the Declaration yet systematically suppressed from the franchise. The contradiction is performative cover for pure extraction of political power. Zero exit option; maximum experienced coercion. The Declaration's universalist rhetoric makes the suppression visible, hence intensifying the extraction mechanism.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAX-QUALIFIED ACTIVE CITIZENS (TANGLED ROPE) — Genuine coordination function: the constitution establishes a framework for collective action, shared legislative authority, and property protection. But this coordination function is asymmetrically structured — the qualification (direct tax payment) ensures the franchise concentrates power among the propertied. Constrained exit through property accumulation or relocation; moderate extraction benefit from the exclusive franchise. Active participation masks the extraction mechanism.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE ASSEMBLY (ROPE) — Institutional beneficiary. The constitutional framework coordinates property protection, orderly succession of laws, and bourgeois institutional consolidation. The assembly experiences the property qualification as a straightforward coordination mechanism: limiting the franchise to taxpayers ensures the legislature represents those with concrete economic interests in the new order. Net beneficiary with institutional arbitrage (can modify constitutional provisions if sufficient support exists).
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SANS-CULOTTES CLUBS AND JACOBIN RADICALS (SCAFFOLD) — Organized agents see the active/passive split as a temporary mistake in the revolutionary project, not a permanent feature. Universal suffrage and the elimination of property qualifications are articulated as the natural endpoint of revolutionary logic. The 1791 Constitution is a transitional form — the sunset is 1793 (first universal male suffrage) or 1795 (Universal Declaration). This perspective experiences low effective extraction because it perceives agency and exit (radical constitutional revision). Sunset clause: the property qualification is structurally unstable under revolutionary pressure.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE KING AS EXECUTIVE MAGISTRATE (TANGLED ROPE) — Louis XVI holds genuine executive coordination power (declaring war, managing courts, supervising administration) but constrained by the Legislative Assembly's budget authority and the suspensive veto (he can delay but not block legislation). The active/passive split serves the monarchy: it ensures the legislature is propertied and potentially moderate, less likely to push radical antimonarchical measures. Constrained exit (flee, attempt restoration, resist) with mixed beneficiary/victim position — coordination mechanism for stable governance alongside extraction of monarchical prerogative.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE PROPERTY QUALIFICATION AS INSTITUTIONAL INERTIA (PITON) — From a civilizational horizon, the property qualification is a degraded residue of ancien régime estate structures, maintained through institutional habit rather than robust functional necessity. The theater ratio (0.35, low for a piton) reflects that the qualification does perform a genuine function (concentrating power among stakeholders), but the theatrical performance — constitutional rhetoric about universal rights coupled with systematic suppression of the franchise — is high. The institution persists through inertia despite the ideological contradiction it has created.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED PROPERTY QUALIFICATION (MOUNTAIN — FALSE SUMMIT) — From a long horizon, this perspective risks reading the property qualification as an inevitable feature of structured governance: that some threshold for political participation is necessary, that universal suffrage leads to chaos, that property is a natural criterion for political voice. This naturalization treats a contingent institutional choice (the specific tax threshold, the specific beneficiary class) as an immutable law of political order. The engine's false summit detector will identify this classification as a naturalized cover story for the contingent extraction visible from the victim perspective.
constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_constitution_1791__active_passive_citizenship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_constitution_1791__active_passive_citizenship, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_constitution_1791__active_passive_citizenship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_constitution_1791__active_passive_citizenship, TR),
    TR >= 0.70.

:- end_tests(french_constitution_1791__active_passive_citizenship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, reflecting the political power asymmetry. The propertied class extracts the exclusive right to shape legislation and thus to shape property law, taxation, and public policy. The extraction is not maximal because the Constitution also establishes stable mechanisms for collective decision-making — the Legislative Assembly does coordinate public goods provision, infrastructure, and dispute resolution. The property qualification concentrates but does not entirely eliminate the coordination function. Rising trajectory (0.52 → 0.65) reflects increasing polarization as peasant unrest and urban mobilization make the exclusion more visible and more contested. Suppression (0.72): High. Multiple mechanisms enforce the property qualification: literacy and administrative barriers to voter registration, property ownership verification, fiscal administration control by the propertied, lack of alternative political channels for the propertyless, and the threat of force (National Guard under propertied command). The Declaration's universality makes the suppression more visible and more resented, actually increasing its intensity as excluded populations recognize they were promised rights yet systematically denied voice. Rising trajectory (0.68 → 0.78) reflects tightening enforcement as the regime faces increasing resistance. Theater ratio (0.35): Moderate-low, reflecting that the property qualification performs a genuine function — it does concentrate power among stakeholders with economic interest in property protection — while carrying high performative content in the gap between Declaration rhetoric and constitutional structure. The theater rises slightly (0.30 → 0.42) as the contradiction becomes more salient and more articulable, but the constraint is not primarily maintained through theater (as a Piton would be) because the extraction mechanism is structural and functional.
 *
 * PERSPECTIVAL GAP:
 *   The active/passive split produces perspectival divergence visible in the classifications. The propertyless experience Snare — they are trapped by systematic suppression with no exit or coordination benefit. The propertied active citizens experience Tangled Rope or Rope — they genuinely coordinate governance while extracting franchise monopoly. The radical Jacobins experience Scaffold — they perceive the property qualification as a transitional mistake that will inevitably be superseded by universal suffrage. The monarchical executive experiences Tangled Rope — constrained by the Assembly yet benefiting from a propertied legislature. The analytical observer at civilizational scope risks naturalizing the property qualification as an inevitable feature of stable government (Mountain, false summit), while the same observer from the victim perspective recognizes it as a contingent institutional choice with clear beneficiaries and victims. The largest gap: between the Declaration's universalist rhetoric (suggesting Rope or even Mountain as natural law) and the Constitution's operative structure (Snare from victim view, Tangled Rope from beneficiary view). This gap is not a measurement error — it is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position. Propertyless victims with trapped exit options receive high d (toward 1.0), producing high f(d) and high experienced extractiveness. Tax-qualified beneficiaries with arbitrage options receive low d (toward 0.15), producing low f(d) and low or negative experienced extractiveness. Radical organizers with mobile exit (they can relocate, join underground clubs, stage insurrection) receive moderate d around 0.55, producing moderate experienced extraction despite being counted as victims structurally. The king with constrained executive options receives moderate-high d, reflecting that the Assembly has stripped executive autonomy. Directionality overrides are not needed — the structural derivation produces appropriate differentiation across the institutional and class positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that all three non-mountain classifications (Snare from victim view, Tangled Rope from beneficiary/executive view, Scaffold from radical view) are legitimate perspectival readings. There is no single true classification — the constraint genuinely exhibits all three patterns depending on structural position. The constraint is NOT a misnamed Rope (which would be mandatrophic) because the tangled/rope distinction is visible only from beneficiary position, whereas victim and radical positions clearly see either pure extraction or transitional coordination. The performative contradiction between Declaration and Constitution is not mandatrophic; it is structural — the Constitution genuinely coordinates governance (Rope/Tangled Rope from beneficiary perspective) while genuinely extracting political voice (Snare from victim perspective). The classification resolves mandatrophy by rejecting the notion of a single true type and embracing the presheaf: the constraint is a Snare-for-the-trapped, a Tangled Rope-for-the-beneficiary, and a Scaffold-for-the-organized, all simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_contradiction,
    'How do the Declaration''s universal rights claims coexist with systematic property-based suppression of the franchise? Is this a genuine contradiction, a transitional compromise, or a hidden power structure naturalizing itself?',
    'Textual analysis of the Declaration vs the Constitution''s implementing articles; historical record of debates in the Constituent Assembly about the property qualification; examination of whether Declaration authors intended universality as aspirational or immediately operative',
    'If the Declaration intended immediate universality: the Constitution is a betrayal (Snare from all victim perspectives, Mountain from beneficiary naturalizes the betrayal). If Declaration intended gradual realization: the Constitution is a coherent transitional form (Tangled Rope with clear Scaffold pressure). If the authors were deliberately vague to manage conflicting factions: the Constitution is a contained contradition (Piton — the institution maintains the performance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_contradiction, conceptual, 'Reconciliation of declared universal rights with property-qualified franchise').

omega_variable(
    property_threshold_magnitude,
    'What proportion of the adult male population met the direct tax qualification? Does this threshold constitute modest targeting (e.g., 40%) or extreme suppression (e.g., <5%)?',
    'Demographic analysis of tax rolls, municipal records of eligible voters, and comparative analysis with ancien régime estate qualifications. Calculation of the franchise percentage by region and urban vs rural distribution.',
    'If >30% of adult males qualified: suppression is moderate (Tangled Rope confirmed). If <10%: suppression approaches total (moves toward Snare). If the proportion was deliberately ambiguous or variable by municipality: the coordination function collapses and the constraint becomes pure extraction (Snare ascendant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_threshold_magnitude, empirical, 'Proportion of adult males meeting the tax qualification').

omega_variable(
    enforcement_mechanism_visibility,
    'Was the property qualification enforced as a rigid gate (systematic exclusion of unqualified voters) or as a fluid norm (de facto suppression with porous boundaries)?',
    'Electoral records from 1791-1792 showing actual voter turnout vs eligible population; testimonies from municipalities about enforcement practices; analysis of whether any unqualified individuals successfully participated',
    'If rigid gate: suppression is structural and high (Snare from victim perspectives). If fluid norm: some agents find arbitrage or mobility (shifts toward Tangled Rope or Rope). If unevenly enforced: coordination collapses and the constraint becomes a cover story for faction (Piton ascendant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_visibility, empirical, 'Rigidity of enforcement of the property qualification').

omega_variable(
    active_passive_rights_difference,
    'What substantive rights did passive citizens retain? Were they full in all domains except voting, or was passive citizenship a partial degradation of legal personhood?',
    'Constitutional text analysis of rights sections; court records and administrative decisions affecting passive citizens; comparison of legal capacities (property ownership, contract, marriage, religious freedom, assembly)',
    'If passive citizens retained full rights except voting: classification leans toward Tangled Rope (suppression focused, extraction narrow). If passive citizenship was a general degradation of legal standing: classification moves toward Snare (structural oppression, not merely franchise restriction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_passive_rights_difference, empirical, 'Substantive rights retained by passive citizens').

omega_variable(
    women_and_servants_explicit_exclusion,
    'Were women and servants explicitly excluded from passive citizenship, or did the text''s ''citizen'' implicitly exclude them through established legal conventions?',
    'Constitutional text (does it say ''man,'' ''male,'' ''head of household''?); contemporary legal interpretation (did jurisprudence treat women and servants as citizen or non-citizen?); constitutional amendment history (when was explicit language added or clarified?)',
    'If explicit exclusion: the constraint is transparently a system of structured oppression (Snare confirmed). If implicit through convention: the constraint is less visible, relying on performative universality (Piton or Tangled Rope with high theater). If the text is ambiguous and interpretation shifted over time: the constraint is a locus of contested reading (the commission reading of the same kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(women_and_servants_explicit_exclusion, conceptual, 'Exclusion status of women and servants from passive citizenship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_constitution_1791__active_passive_citizenship, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr1791_apcs_theater_t0, french_constitution_1791__active_passive_citizenship, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fr1791_apcs_theater_t6, french_constitution_1791__active_passive_citizenship, theater_ratio, 6, 0.35).
narrative_ontology:measurement(fr1791_apcs_theater_t12, french_constitution_1791__active_passive_citizenship, theater_ratio, 12, 0.42).

% Extraction over time
narrative_ontology:measurement(fr1791_apcs_ext_t0, french_constitution_1791__active_passive_citizenship, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fr1791_apcs_ext_t6, french_constitution_1791__active_passive_citizenship, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fr1791_apcs_ext_t12, french_constitution_1791__active_passive_citizenship, base_extractiveness, 12, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fr1791_apcs_supp_t0, french_constitution_1791__active_passive_citizenship, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(fr1791_apcs_supp_t6, french_constitution_1791__active_passive_citizenship, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(fr1791_apcs_supp_t12, french_constitution_1791__active_passive_citizenship, suppression_requirement, 12, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_constitution_1791__active_passive_citizenship, enforcement_mechanism).
narrative_ontology:affects_constraint(french_constitution_1791__active_passive_citizenship, french_constitution_1791__declaration_of_rights_1789).
narrative_ontology:affects_constraint(french_constitution_1791__active_passive_citizenship, french_constitution_1791__suspensive_veto_monarchy).
narrative_ontology:affects_constraint(french_constitution_1791__active_passive_citizenship, french_constitution_1791__failure_and_succession).

% DUAL FORMULATION NOTE:
% The 1791 Constitution is a contested kernel with multiple structurally distinct readings, each producing different constraint classifications. This story (active_passive_citizenship) focuses on the extraction mechanism indexed to property qualification and the systematic suppression of the franchise from the propertyless majority. Sibling readings decompose the same constitutional text along different axes: declaration_of_rights examines the universalist rhetoric and its contradictions; suspensive_veto examines the retention of monarchical executive power; failure_and_succession examines the temporal instability and rapid collapse of the constitutional form. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and measurements. They are linked by the shared kernel (the 1791 Constitution) and the declaration_of_rights story forwards to all others through causality (the Declaration's universality is what creates the contradiction that active_passive_citizenship operationalizes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_constitution_1791__active_passive_citizenship, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
