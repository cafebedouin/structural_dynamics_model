% ============================================================================
% CONSTRAINT STORY: amendment_and_basic_structure__first_amendment_1951
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_and_basic_structure__first_amendment_1951, []).

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
 *   constraint_id: amendment_and_basic_structure__first_amendment_1951
 *   human_readable: First Amendment (1951): Constitutional Speech Restrictions and Ninth Schedule Land Reform Insulation
 *   domain: constitutional_law/doctrinal_amendment
 *
 * SUMMARY:
 *   The First Amendment (1951), adopted within fifteen months of the
 *   Constitution's adoption (January 1950), represents the founding
 *   generation amending its own work against its own courts. The amendment
 *   imposed two structural changes: (1) it restricted the scope of freedom of
 *   speech, assembly, and association (Article 19), permitting 'reasonable
 *   restrictions' for state security, public order, and morality; (2) it
 *   introduced the Ninth Schedule, which placed specified laws outside the
 *   scope of judicial review under Articles 31 (property rights) and other
 *   fundamental rights. The first major law shielded by the Ninth Schedule
 *   was the Agriculture Estate Abolition Act, which redistributed zamindari
 *   (landlord) holdings to peasants and smallholders. The constraint captures
 *   the tension between the coordinate land reform program and the
 *   suppression of both property-rights challenge and broad speech doctrine.
 *   The amendment was adopted because early court decisions (particularly the
 *   high court ruling in the zamindari cases) began to strike down land
 *   reform legislation as violating the right to property and freedom of
 *   expression. The founding generation responded by amending the
 *   Constitution itself—not to clarify an ambiguity but to reverse an outcome
 *   they opposed. This is a core moment in the history of constitutional
 *   contestation: does the people's amendment power supersede the judicial
 *   power, or does the Constitution bind even amendment? Kesavananda Bharati
 *   (1973) would later hold that the answer is both—Parliament may amend
 *   freely, but amendment cannot destroy the 'basic structure' of the
 *   Constitution. But in 1951, that limit did not exist. The First Amendment
 *   was unconstrained.
 *
 * KEY AGENTS:
 *   - Newly Independent Legislative State: Primary beneficiary (institutional/arbitrage) — Parliament designs the amendment and gains the power to insulate redistribution from judicial review; direct control over the amendment mechanism
 *   - Land Reform Beneficiaries (Peasants, Smallholders): Secondary beneficiary (institutional/arbitrage in aggregate, though individual smallholders have low power) — gain secure land tenure insulated from property-based challenge; benefit from state coordination of redistribution
 *   - Zamindari Property Holders: Primary victim (powerless/trapped) — face expropriation of holdings with limited compensation, insulated from judicial review by the Ninth Schedule; prior constitutional right to property is erased by amendment itself
 *   - Broad Speech Doctrine (Doctrinal Victim): Abstract victim (powerless/trapped) — freedom of expression is narrowed, and the restriction is constitutionalized, binding future interpreters; no escape mechanism
 *   - Judicial Review Authority: Secondary victim (organized/constrained) — courts lose authority over land reform and other Ninth Schedule matters; review power is explicitly curtailed; also constrained in broader speech jurisprudence
 *   - Legal Academy / Constitutional Doctrine: Secondary victim (organized/constrained) — scholarly interpretation cannot overturn the Ninth Schedule barrier; doctrinal development in property and speech is cut off by constitutional text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_and_basic_structure__first_amendment_1951, 0.48).
domain_priors:suppression_score(amendment_and_basic_structure__first_amendment_1951, 0.68).
domain_priors:theater_ratio(amendment_and_basic_structure__first_amendment_1951, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_and_basic_structure__first_amendment_1951, extractiveness, 0.48).
narrative_ontology:constraint_metric(amendment_and_basic_structure__first_amendment_1951, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(amendment_and_basic_structure__first_amendment_1951, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_and_basic_structure__first_amendment_1951, tangled_rope).
narrative_ontology:human_readable(amendment_and_basic_structure__first_amendment_1951, "First Amendment (1951): Constitutional Speech Restrictions and Ninth Schedule Land Reform Insulation").
narrative_ontology:topic_domain(amendment_and_basic_structure__first_amendment_1951, "constitutional_law/doctrinal_amendment").

domain_priors:requires_active_enforcement(amendment_and_basic_structure__first_amendment_1951).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_and_basic_structure__first_amendment_1951, 'ac74de6e-9caa-4047-9af6-35df3ed5303b').
narrative_ontology:cs_kernel_codification('ac74de6e-9caa-4047-9af6-35df3ed5303b', formalized).
narrative_ontology:cs_authority_grounding('ac74de6e-9caa-4047-9af6-35df3ed5303b', lineage).
narrative_ontology:cs_interpretation_layer_present('ac74de6e-9caa-4047-9af6-35df3ed5303b').
narrative_ontology:cs_reading_relation('ac74de6e-9caa-4047-9af6-35df3ed5303b', amendment_and_basic_structure__forty_second_amendment_1976, influences).
narrative_ontology:cs_reading_relation('ac74de6e-9caa-4047-9af6-35df3ed5303b', amendment_and_basic_structure__forty_fourth_amendment_1978, influences).
narrative_ontology:cs_reading_relation('ac74de6e-9caa-4047-9af6-35df3ed5303b', amendment_and_basic_structure__kesavananda_basic_structure, coexists_with).
narrative_ontology:cs_axiom('ac74de6e-9caa-4047-9af6-35df3ed5303b', foundational, parliamentary_amendment_sovereign).
narrative_ontology:cs_axiom_status(parliamentary_amendment_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('ac74de6e-9caa-4047-9af6-35df3ed5303b', parliamentary_amendment_sovereign, deontological).
narrative_ontology:cs_axiom('ac74de6e-9caa-4047-9af6-35df3ed5303b', foundational, property_rights_justiciably_revisable).
narrative_ontology:cs_axiom_status(property_rights_justiciably_revisable, overridden).
narrative_ontology:cs_axiom_grounding('ac74de6e-9caa-4047-9af6-35df3ed5303b', property_rights_justiciably_revisable, empirically_contingent).
narrative_ontology:cs_reference_frame('ac74de6e-9caa-4047-9af6-35df3ed5303b', constituent_power_supremacy).
narrative_ontology:cs_drift_state('ac74de6e-9caa-4047-9af6-35df3ed5303b', post_kesavananda_basic_structure_jurisprudence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ac74de6e-9caa-4047-9af6-35df3ed5303b', '').
narrative_ontology:cs_kernel_id(amendment_and_basic_structure__first_amendment_1951, amendment_and_basic_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_and_basic_structure__first_amendment_1951, legislative_land_reform_program).
narrative_ontology:constraint_beneficiary(amendment_and_basic_structure__first_amendment_1951, newly_independent_state).
narrative_ontology:constraint_victim(amendment_and_basic_structure__first_amendment_1951, zamindari_property_holders).
narrative_ontology:constraint_victim(amendment_and_basic_structure__first_amendment_1951, broad_speech_doctrine).
narrative_ontology:constraint_victim(amendment_and_basic_structure__first_amendment_1951, judicial_review_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ZAMINDARI PROPERTY HOLDERS (SNARE) — Landed interests face total suppression of judicial challenge via the Ninth Schedule amendment lock-out. Cannot exit through courts; property redistribution is insulated from review. Maximum extraction: prior rights recognized in Constitution are erased by the amendment itself. No alternatives for defense of property claims against legislative action.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BROAD SPEECH DOCTRINE / ABSTRACT VICTIM (SNARE) — Constitutional protection of speech is narrowed via First Amendment restrictions (reasonable restrictions on speech, assembly, association). The doctrine cannot escape; freedom of expression doctrine is structurally weakened. Future generations inherit a restricted baseline. Zero exit; trapped in the narrower regime.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL REVIEW AUTHORITY (TANGLED ROPE) — Courts benefit from being positioned as implementers of constitutional order; their legitimacy derives from enforcing the Constitution. But they face suppression via the Ninth Schedule amendment barrier — their review power over land reform is explicitly curtailed. Mixed: courts gain a stable role in the new order but lose authority over redistributive legislation. Extraction exists but is asymmetric: courts accept narrowed scope in exchange for stability in other domains.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NEWLY INDEPENDENT LEGISLATIVE STATE (ROPE) — Parliament benefits from the amendment: speech restrictions enable coordinated messaging for the nation-building project; Ninth Schedule insulation enables land reform without judicial interference. The amendment is a coordination mechanism for the state's redistributive agenda. The state has arbitrage capacity — it designed the amendment and can refine it. Net beneficiary: extraction flows toward the state apparatus.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LAND REFORM BENEFICIARIES (ROPE) — Peasants and smallholders benefit from redistribution insulated from property-rights challenge. The amendment enables coordinated redistribution and shields it from judicial unraveling. This is genuine coordination: the state and peasant interests align on insulating reform from court intervention. Low suppression of the beneficiary group itself — they experience the amendment as protective, not coercive.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL DOCTRINE / LEGAL ACADEMY (TANGLED ROPE) — Legal scholars and the interpretive community face suppression via the Ninth Schedule: specific texts are declared outside review, cutting off doctrinal development in those areas. They also benefit from the stability the amendment provides: the Constitution is clarified, not dissolved. Mixed experience: suppressed in scope of permissible interpretation; enabled in predictability of the basic rules. Constrained exit: academics can critique but cannot reverse the amendment through doctrinal work alone.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY IMMUTABILITY (MOUNTAIN) — From the civilizational analytical view, a sovereign people must retain the power to amend their constitution without external constraint. The amendment power itself cannot be subject to judicial veto — that would subordinate the constitution-maker to the constitution. From this frame, the First Amendment appears as an immutable feature of constitutional sovereignty: the people, speaking through Parliament, can restrict speech and insulate redistribution because the people's will expressed through amendment supersedes prior constitutional text. This perspective risks naturalizing what is a contingent power grab.
constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_and_basic_structure__first_amendment_1951_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_and_basic_structure__first_amendment_1951, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amendment_and_basic_structure__first_amendment_1951, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(amendment_and_basic_structure__first_amendment_1951_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The First Amendment extracts from multiple victim classes: zamindaris lose property rights and judicial remedy; speech doctrine loses scope; courts lose review authority. But the extraction is not maximal (hence not 0.66+ snare) because it is paired with a genuine coordination function for the land reform beneficiaries. The amendment solves a real collective action problem: without Ninth Schedule protection, land reform would be continuously unraveled by litigation. The state and peasant interests genuinely align on insulating redistribution. This hybrid character (coordination + asymmetric extraction) justifies the tangled_rope classification. Suppression (0.68): High. The Ninth Schedule creates an absolute barrier to judicial challenge—not a high cost, but a zero-cost ceiling. Property holders cannot petition courts; speech restrictions are constitutionally entrenched. The suppression is structural: alternatives (property rights, broad speech doctrine, judicial review) are not closed off by law but are sealed off by the amendment itself. The amendments also include provisions enabling detention and restricting dissent, raising suppression of alternative voices. Theater ratio (0.52): Moderate. The amendment is not primarily performative—it does real work insulating land reform from judicial unraveling. But it also has a theatrical dimension: the state invokes the 'socialist' project and nation-building to justify speech restrictions that go beyond land reform itself. The restriction on assembly and association serves state consolidation as much as land redistribution. The measurements show increasing theater over time (0.38 → 0.52 → 0.58) as the land reform stabilizes and the speech restrictions remain, suggesting the theater of state security becomes the dominant function as the original coordination problem is solved.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extraordinary perspectival diversity because the amendment's effects are radically asymmetric across agents. Zamindaris see a snare: property rights are erased and review is foreclosed. Courts see tangled_rope: they gain stability in their role but lose authority over redistribution. The state sees rope: the amendment coordinates the land reform project and enables state consolidation. The doctrinal victim (broad speech doctrine) sees snare: the restriction is constitutionalized and binding on interpretation. The analytical observer risks seeing mountain: amendment power is sovereign; the people cannot be constrained by their own prior constitution. But this mountain perspective naturalizes what is a specific political choice: the founding generation could have created a basic structure doctrine limiting amendment (as Kesavananda later tried to do), but they chose not to. The perspectival gap reveals the kernel contest: is the amendment an expression of popular sovereignty that cannot be questioned, or is it a contingent power grab that should be subject to basic structure limits?
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the extractive flow. The state (institutional/arbitrage) benefits from the amendment and designed it; d is low (≈0.15), producing negative f(d) and net benefit. Zamindaris (powerless/trapped) have no exit; d approaches 1.0, producing f(d) ≈ 1.42 and maximum experienced extraction. The judicial authority (organized/constrained) faces costs (loss of review power) but also gains (institutional stability); d is moderate (≈0.55), producing mixed extraction. The land reform beneficiaries (moderate/mobile, though individually dispersed) experience the amendment as protective; their d is low despite being victims in the abstract sense, because they are the intended beneficiaries of the suppression. The broad speech doctrine as a victim is abstract and powerless; its d is high (≈0.85), but the constraint operates on doctrine rather than individual agents. The analytical observer (analytical/analytical) occupies a meta-position where d is determined by canonical fallback (≈0.73), but the observer risks adopting the sovereigntist framing that erases the extraction entirely by naturalizing it as immutable law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_structure_doctrine_timing,
    'Did the First Amendment (1951) establish the principle that Parliament cannot amend basic structure, or did Kesavananda Bharati (1973) invent this doctrine retroactively, making the 1951 amendment a contingent political choice rather than a constitutional necessity?',
    'Doctrinal genealogy: examination of pre-Kesavananda commentary on amendment power; analysis of whether the 1951 framers believed they faced justiciable limits on amendment; comparison with other constitutional systems'' amendment practices in 1950-1951.',
    'If basic structure was always a constraint: the First Amendment suppression is extraction within justified limits. If Kesavananda invented the doctrine: the 1951 amendment is a naked power grab that later doctrine tried to legitimize retroactively. Classification shifts from constrained-extraction (tangled_rope) to unrestricted-extraction (snare) if no prior doctrinal limit existed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_structure_doctrine_timing, conceptual, 'Whether basic structure doctrine pre-existed the First Amendment or was invented by Kesavananda').

omega_variable(
    zamindari_compensation_adequacy,
    'Were zamindari property holders given fair market compensation under the land reform scheme, or was the Ninth Schedule insulation a vehicle for confiscatory redistribution?',
    'Historical analysis of compensation levels relative to pre-1951 property values; comparison with international land reform programs; examination of whether compensation followed due process norms or was administratively determined without judicial check.',
    'If compensation was adequate: extraction is moderate, justified by legitimate redistribution (tangled_rope confirmed). If compensation was inadequate and unjusticiable: extraction is severe, and the constraint shifts toward snare. The Ninth Schedule''s function—whether protecting fair redistribution or enabling confiscation—determines whether suppression of judicial review was extraction or coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zamindari_compensation_adequacy, empirical, 'Whether zamindari compensation was adequate or confiscatory').

omega_variable(
    speech_restrictions_functional_necessity,
    'Were the First Amendment speech restrictions functionally necessary for the state-building project and land reform implementation, or were they ideological tools for suppressing dissent unrelated to redistribution?',
    'Comparative analysis: examination of other newly independent states that implemented land reform without equivalent speech restrictions; historical analysis of which speech was actually restricted and whether restriction targeted reform opponents or broader dissent; measurement of speech restriction deployment during actual reform implementation vs. during consolidation phases.',
    'If restrictions were functionally necessary: they represent coordination for collective action (tangled_rope). If restrictions were ideological suppression: they represent pure extraction (snare). The amendment''s theater_ratio depends on whether restrictions served functional coordination or performative state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_restrictions_functional_necessity, empirical, 'Whether speech restrictions were functionally necessary for land reform').

omega_variable(
    amendment_power_sovereignist_versus_limited,
    'Is the amendment power an expression of sovereign will that cannot be constrained by prior constitutional text, or is the constitution a supreme law that can constrain even amendment through the basic structure doctrine?',
    'Philosophical/jurisprudential analysis: comparison of different traditions (Hobbesian sovereigntist vs. constitutionalist); examination of whether Kesavananda''s basic structure doctrine is defensible or is a judicial power grab; analysis of how other constitutional systems handle amendment limits.',
    'If sovereigntist view is correct: the First Amendment is a legitimate exercise of constituent power; the mountain perspective is vindicated. If constitutionalist view is correct: the First Amendment is an unconstitutional amendment that violates basic structure; the reading becomes a snare throughout (no legitimate coordination function). This omega resolves the deepest doctrinal contest between the first_amendment_1951 reading and the kesavananda_basic_structure reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_power_sovereignist_versus_limited, conceptual, 'Sovereigntist vs. constitutionalist view of amendment power').

omega_variable(
    zamindari_class_reconstruction,
    'After the Ninth Schedule insulation of land reform, did zamindari interests successfully capture regulatory space (tenancy law, agricultural commodity marketing, input credit systems) to reconstruct economic dominance, or was the redistribution irreversible?',
    'Historical-institutional analysis: tracking of post-1951 landownership concentration; analysis of whether zamindari networks moved into moneylending, agricultural trade, or land tenure regulation; examination of whether the Ninth Schedule insulation remained effective or was eroded through regulatory mechanisms outside its scope.',
    'If reconstruction occurred: the First Amendment suppression was temporally limited — extraction was front-loaded but not permanent. The constraint''s lifecycle shows declining extractiveness as the beneficiary class adapted. If reconstruction did not occur: extraction was durable, confirming the snare classification for property holders. Measurements should show whether suppression_requirement changed over time as the reform became normalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zamindari_class_reconstruction, empirical, 'Whether zamindari interests reconstructed dominance after land reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_and_basic_structure__first_amendment_1951, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_pre_amendment, amendment_and_basic_structure__first_amendment_1951, theater_ratio, 0, 0.38).
narrative_ontology:measurement(theater_t3_post_first_amendment, amendment_and_basic_structure__first_amendment_1951, theater_ratio, 3, 0.52).
narrative_ontology:measurement(theater_t10_consolidation, amendment_and_basic_structure__first_amendment_1951, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(extr_t0_pre_amendment, amendment_and_basic_structure__first_amendment_1951, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extr_t3_post_first_amendment, amendment_and_basic_structure__first_amendment_1951, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(extr_t10_accumulation, amendment_and_basic_structure__first_amendment_1951, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(supp_t0_pre_amendment, amendment_and_basic_structure__first_amendment_1951, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(supp_t3_post_first_amendment, amendment_and_basic_structure__first_amendment_1951, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(supp_t10_hardened, amendment_and_basic_structure__first_amendment_1951, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_and_basic_structure__first_amendment_1951, enforcement_mechanism).
narrative_ontology:affects_constraint(amendment_and_basic_structure__first_amendment_1951, forty_second_amendment_1976).
narrative_ontology:affects_constraint(amendment_and_basic_structure__first_amendment_1951, forty_fourth_amendment_1978).
narrative_ontology:affects_constraint(amendment_and_basic_structure__first_amendment_1951, kesavananda_basic_structure).

% DUAL FORMULATION NOTE:
% The First Amendment constraint documents the specific mechanism of the 1951 amendment (speech restrictions + Ninth Schedule insulation). It is the upstream reading in the amendment family. The downstream constraints (forty_second and forty_fourth amendments) are triggered by and respond to the structural principles established here. Kesavananda_basic_structure (the judicial counter-reading) attempts to limit the doctrine the First Amendment inaugurated. Each reading has its own epsilon and its own perspectives; the network edges map the doctrinal influence chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_and_basic_structure__first_amendment_1951, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
