% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Woman/Female Category Membership via Sex Biology (XX/XY Chromosomal Sex and Reproductive Anatomy)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The sex-biology reading of woman/female category membership treats
 *   chromosomal sex (XX/XY) and reproductive anatomy (ovaries/uterus/vagina,
 *   sperm/testis/penis) as the criteria that determine female category
 *   membership. The reading is not primarily about identity or recognition —
 *   it is about biological fact as the ground of social category. From this
 *   reading's perspective, the category 'female' is defined by the biological
 *   reproductive role (gamete production, gestation capacity) and
 *   developmental biology (ovarian development, typical female-pattern
 *   differentiation), not by how a person identifies. The reading is
 *   politically significant because it grounds sex-segregated spaces
 *   (bathrooms, shelters, prisons, locker rooms, intimate medical settings,
 *   athletic categories) in an appeal to nature — the boundary is not a
 *   social convention but a biological fact. This framing appears to make the
 *   boundary immutable and beyond political contest. However, the structural
 *   data reveals a more complex reality: the reading requires active
 *   institutional enforcement (verification procedures, boundary policing,
 *   legal gatekeeping); it produces victims (trans women denied access to
 *   female-only spaces, intersex persons rendered invisible or
 *   miscategorized); it benefits identifiable groups (natal females seeking
 *   safety protections, institutions seeking simple classification rules);
 *   and it faces an empirical limit (intersex conditions demonstrate that
 *   XX/XY is not universally exhaustive). These structural facts indicate
 *   that the sex-biology reading is not a discovered natural law but a
 *   contestable institutional arrangement that coordinates some functions
 *   (safety in vulnerable contexts) while extracting costs (exclusion,
 *   enforcement, denial of intersex variation). The tangled_rope
 *   classification captures this duality: the reading is neither pure
 *   coordination (it excludes and harms) nor pure extraction (it does protect
 *   natal females in actual vulnerability). The tension between the reading's
 *   naturalizing framing (chromosomal sex as immutable fact) and its
 *   structural extractiveness is the core mandatrophy the constraint
 *   embodies.
 *
 * KEY AGENTS:
 *   - Trans women: Primary victim (powerless/trapped) — structurally excluded from female-only spaces and institutions by a reading that defines them as 'not female' by chromosomal definition. No exit mechanism, no agency, no arbitrage.
 *   - Intersex persons (XXY, X0, XX males, XY females, chimeric): Primary victim (powerless/trapped) — rendered invisible or miscategorized by a reading that presupposes XX/XY as exhaustive. Faces forced assignment to wrong category, denial of appropriate medical care, exclusion from identity-affirming spaces.
 *   - Natal females in sex-segregated safety contexts (prisons, shelters, intimate medical care): Primary beneficiary (moderate/constrained) — experience the reading as coordinating genuine safety functions in contexts of vulnerability (sexual violence risk, intimate bodily exposure). Also constrained by enforcement costs and by the reading's inability to distinguish trans women by actual risk factors (institutional history, socialization, violence training) rather than chromosomal sex.
 *   - Sex-based rights advocacy organizations: Organized beneficiary (organized/constrained) — maintain the reading's institutional enforcement, organize spaces around sex-biology criteria, benefit from stable boundaries. Also constrained by legal challenge, internal contradiction when intersex members arise, and the enforcement labor required.
 *   - Legal and institutional systems (courts, prisons, healthcare, sports, census): Institutional beneficiary (institutional/arbitrage) — experience the reading as simple, administratively useful coordination mechanism. Use XX/XY as classification criterion for policy implementation, facility design, resource allocation.
 *   - Medical and biological scientific authorities: Piton perspective (institutional/arbitrage) — historically maintained the reading as scientific fact but now operate within developmental biology understanding of sex as multiply-determined (chromosomal, gonadal, hormonal, neurological) and non-universally binary. Persist in using XX/XY in policy while research sophistication demonstrates the boundary's limits.
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks treating the reading as discovered natural law when it is actually a contestable institutional arrangement grounded in naturalizing rhetoric.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.58).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.68).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Woman/Female Category Membership via Sex Biology (XX/XY Chromosomal Sex and Reproductive Anatomy)").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '439f1bf5-c19a-4db2-85cb-f3d1cf200b6d').
narrative_ontology:cs_kernel_codification('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', fixed_text).
narrative_ontology:cs_authority_grounding('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', extraction).
narrative_ontology:cs_interpretation_layer_present('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d').
narrative_ontology:cs_reading_relation('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', foundational, biological_sex_category_determinant).
narrative_ontology:cs_axiom_status(biological_sex_category_determinant, holdable).
narrative_ontology:cs_axiom_grounding('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', biological_sex_category_determinant, empirically_contingent).
narrative_ontology:cs_axiom('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', foundational, sex_category_immutable_across_contexts).
narrative_ontology:cs_axiom_status(sex_category_immutable_across_contexts, holdable).
narrative_ontology:cs_axiom_grounding('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', sex_category_immutable_across_contexts, empirically_contingent).
narrative_ontology:cs_axiom('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', secondary, excludes_identity_independent_category_membership).
narrative_ontology:cs_axiom_status(excludes_identity_independent_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', excludes_identity_independent_category_membership, deontological).
narrative_ontology:cs_reference_frame('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', contemporary_identity_recognition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('439f1bf5-c19a-4db2-85cb-f3d1cf200b6d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, female_bodied_persons_in_safety_contexts).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_only_spaces).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, non_binary_persons_with_female_biology).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_persons_outside_xy_xx_binary).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMEN (SNARE) — Cannot exit the category boundary imposed by chromosomal/anatomical criteria. Trapped by immutable definitions that deny access to female-only spaces, shelters, prisons, bathrooms, sports categories. Maximum experienced extraction — the reading creates categorical disability with no exit mechanism. The power asymmetry is extreme: the reading does not say 'trans women may participate in some female contexts'; it says 'trans women are, by definition, not female.' No agency, no arbitrage, no recourse.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERSEX PERSONS (SNARE) — Equally trapped by a reading that presupposes XX/XY as exhaustive. Persons with Klinefelter (XXY), Turner (X0), De la Chapelle (XX male), or other chromosomal variations are rendered invisible or forced into male categories despite female reproductive anatomy (or vice versa). The reading claims to ground identity in biological fact but actually imposes a binary that biology contradicts. Extraction: exclusion, misgendering, denial of appropriate medical care.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NATAL FEMALES SEEKING SEX-BASED PROTECTIONS (TANGLED ROPE) — Experiences coordination and extraction simultaneously. The reading coordinates genuine safety functions: sex-segregated prisons, shelters, and intimate medical care settings exist to protect people from sexual violence in situations of vulnerability. The reading's beneficiary is the agent who benefits from exclusion of those with male anatomy and/or socialization in contexts where physical vulnerability is extreme. But the constraint also extracts: it requires constant enforcement (legal boundaries, verification procedures, institutional gatekeeping), it denies access to trans women and intersex persons, and it presupposes a binary biology that does not universally hold. The agent experiences both the coordination gain (safety) and the extractive cost (enforcement, boundary policing, exclusion of others).
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SEX-BASED RIGHTS ADVOCATES (TANGLED ROPE) — Organized agents (women's shelters, female-only organizing spaces, sex-based rights advocacy groups) see the reading as coordinating genuine safety and solidarity. The coordination function is real: these spaces enable discussion of pregnancy, menstruation, sexual coercion, and reproductive autonomy without the male presence or socialization that shapes institutional space. But organized advocates also perpetuate extraction: they enforce boundaries through institutional gatekeeping, they deny voice to trans women and intersex persons, and they treat chromosomal sex as immutable ground when biology is more complex. The advocacy coalition has power and agency — they are not victims — but their power is leveraged through the reading's enforced boundaries, which means their agency inherently extracts.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL CLASSIFICATION (ROPE) — From the perspective of legal and institutional systems (courts, prison systems, healthcare authorities, sports bodies, census systems), the sex-biology reading is a coordination mechanism: it provides a stable, administratively simple criterion for categorization that enables policy implementation, risk assessment, and resource allocation. Institutions can design sex-segregated facilities, adjust medical protocols, and organize athletics without constantly negotiating identity questions. The reading experiences this as pure coordination — the reading solves an administrative problem. But institutional perspective is beneficiary perspective: institutions benefit from stable rules, and the reading's simplicity hides the extraction it produces elsewhere.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: BIOLOGICAL ESSENTIALISM (PITON) — The historical medical and biological framing of sex as chromosomal/anatomical essence has become largely performative. Modern developmental biology understands sex as a complex product of chromosomal, gonadal, hormonal, and neurological differentiation with multiple sites of variation and non-binary outcomes. The XX/XY reading persists not because it captures modern biological understanding but because institutional inertia, public comprehensibility, and legal convenience maintain it. Medical authorities cite chromosomal sex in policy while their own research contradicts the binary. Theater ratio 0.35 reflects that the reading's biological face is maintained for institutional and political purposes despite scientific sophistication showing the limits of chromosomal sex as a category boundary. The reading persists through authority maintenance, not empirical robustness.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, chromosomal sex (XX/XY) is an objective biological fact, immutable and universal, grounding the category boundary in nature itself rather than social convention. This perspective treats the sex-biology reading as discovering a pre-social fact about human biology. The reading appears immutable because it claims to be grounded in genetic/chromosomal reality. However, this perspective's mountain classification is contested by the structural data: the reading requires active enforcement, it produces victims (trans women, intersex persons), it benefits identifiable groups (natal females in specific contexts, institutions seeking simple rules), and intersex biology demonstrates that XX/XY is not universally exhaustive. The engine's false summit detector will likely flag this perspective, revealing that the 'natural law' framing naturalizes what is actually a contestable institutional arrangement.
constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(woman_female_category__sex_biology_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(woman_female_category__sex_biology_reading, TR),
    TR >= 0.70.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading coordinates genuine safety functions in contexts of extreme vulnerability (sexual violence risk in prisons and shelters), which provides real coordination benefit. But the coordination comes packaged with extraction: trans women are absolutely denied access (no cost-benefit trade-off possible, just exclusion), intersex persons are miscategorized or erased, and the boundary requires institutional enforcement labor. The extractiveness score reflects that the reading is not pure extraction — some genuine safety coordination exists — but the extraction is substantial and non-negotiable. The rising trajectory (0.42 → 0.58) models the reading's intensification over the measurement interval: as legal and political contests have sharpened the boundary, enforcement requirements have grown, and the reading has become more explicit about what it excludes rather than implicit or negotiable. Suppression (0.68): High. The reading closes off alternatives: if you are XX, you are female; if you are not XX, you are not female (regardless of anatomy or identity). The closure is enforced through institutional gatekeeping (legal boundaries, verification procedures, bathroom/facility access rules, sports eligibility rules). Intersex persons face particularly severe suppression — the reading renders them invisible or forces them into the wrong category, and there is no institutional pathway to exit or modify the assignment. The rising trajectory (0.55 → 0.68) models increasing enforcement intensity and legal hardening of boundaries over the interval. Theater ratio (0.35): Low. The reading's biological face is largely non-performative — chromosomal sex is genuinely used as stated criterion for classification. The theater that exists (0.35 rather than 0.0) reflects that institutions often do not actually verify chromosomal status; they rely on appearance, legal documentation, and self-declaration, which means the XX/XY criterion is applied somewhat performatively (institutions cite it as ground of the boundary but often do not rigorously verify it). The slight increase (0.28 → 0.35) over the interval models a shift toward more explicit verification procedures and institutional policing as the reading has become politically contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence along the powerless/institutional axis. Trans women (powerless/trapped) experience absolute exclusion and recategorization as 'not female' — the reading creates a categorical disability with no exit. Intersex persons experience similar exclusion coupled with invisibility — the reading presupposes a binary that does not describe their biology. Natal females in safety contexts (moderate/constrained) experience coordination and protection — sex-segregated spaces enable solidarity, reduce sexual violence risk, and allow discussion of reproduction and bodily autonomy without male presence. Organized sex-based rights advocates (organized/constrained) extend this coordination benefit but also experience enforcement burden — they must police boundaries, defend against legal challenge, and manage edge cases where the binary breaks down. Institutions (institutional/arbitrage) experience the reading as administratively simple coordination — one clear rule for classification, facility design, resource allocation, and policy implementation. Medical authorities (institutional/arbitrage, piton perspective) maintain the reading despite developmental biology understanding that contradicts it — the reading persists through inertia and institutional convenience rather than empirical robustness. The analytical observer (analytical/analytical) risks naturalizing the reading as biological fact, missing the structural extraction that the reading produces. The perspectival gap is not a disagreement about what chromosomal sex IS — biological facts are what they are. The gap is about whether the XX/XY boundary is an adequate criterion for female category membership, and whether the coordination benefits justify the extraction costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures where each agent sits relative to the extraction flow of this constraint. Trans women have d ≈ 1.0 (full target): they have no beneficiary position in this reading, maximal exit barriers (trapped exit status), and experience pure extraction (absolute exclusion). Intersex persons have d ≈ 0.95 (near-full target): similarly trapped and excluded, with some marginal benefit if they happen to have anatomy that aligns with one category or another. Natal females in safety contexts have d ≈ 0.45 (slight beneficiary leaning): they benefit from the reading's coordination (safety, solidarity, bodily autonomy in vulnerable contexts) but also bear extraction costs (enforcement labor, boundary maintenance, the reading's internal contradiction when dealing with intersex members or edge cases). Sex-based advocates have d ≈ 0.38 (beneficiary leaning): they benefit from the reading's institutional validation and use it to organize space and power, but constrained exit options (facing legal challenge, needing to defend the reading) mean they are not pure beneficiaries with arbitrage. Institutions have d ≈ 0.25 (beneficiary with arbitrage): they benefit substantially from the reading's simplicity and use it for administrative purposes, and they have high exit options (they can change classification criteria, as some have done; they can accommodate intersex persons; they can use alternative criteria). Medical authorities have d ≈ 0.20 (institutional beneficiary with arbitrage): they benefit from the reading's ability to anchor policy in 'nature' but have the power to revise it and have begun to do so; their maintained use of the reading despite contrary research indicates institutional inertia (piton mechanism) rather than strong benefit. The analytical observer has d ≈ 0.72 (analytical observer canonical value): observing all perspectives, the observer's structural relationship to the extraction flow is neither beneficiary nor victim but measured distance.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANDATROPHY: This reading resolves the mandatrophy by making its own commitments explicit and documenting the structural cost of those commitments. The reading's core claim: category membership is determined by XX/XY chromosomal sex and reproductive anatomy, immutable and grounded in nature. This commitment provides real coordination benefits (safety protections in vulnerable contexts, clear rules for institutional implementation) but requires exclusion of trans women and invisibility/miscategorization of intersex persons. The mandatrophy is not 'which reading is correct?' but 'what are the structural costs of grounding the boundary in XX/XY?' The reading is neither pure coordination (it produces victims and requires enforcement) nor pure extraction (it does protect natal females in real vulnerability). The tangled_rope classification holds both simultaneously: the reading coordinates some functions while extracting costs from others. The false summit is the reading's own naturalization: it claims to discover an immutable biological fact when the structural data reveals a contestable institutional arrangement (XX/XY is not universally exhaustive; the coordination benefit is real but context-specific; enforcement is required; alternative criteria exist and are used in some contexts). The rising extractiveness trajectory reflects that intensification of boundary policing increases extraction costs. The sibling readings (gender_identity_reading, hybrid_contextual_reading) represent alternatives that would lower extraction in different dimensions: identity-based reading would reduce exclusion of trans women but might reduce safety coordination; contextual reading would enable XX/XY in safety contexts while allowing identity-based recognition elsewhere, reducing overall extraction but requiring institutional complexity and case-by-case negotiation. The constraint story documents that the sex-biology reading is not a discovered natural law but a living choice with structural consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chromosomal_exhaustiveness_ambiguity,
    'Does XX/XY chromosomal sex constitute an exhaustive biological category boundary, or do intersex conditions (XXY, X0, XX males, XY females, chimerism) demonstrate that chromosomal sex is one among multiple biological axes?',
    'Systematic epidemiological census of intersex prevalence; empirical determination of what percentage of humans fall outside XX/XY binary; analysis of whether category boundary remains administratively useful at actual intersex prevalence',
    'If chromosomal sex is exhaustive: the reading captures biological reality cleanly. If intersex persons constitute a meaningful percentage and exhibit biological features of both XX and XY categories (or neither): the category boundary is empirically inadequate, and the reading''s claim to ground the boundary in ''immutable biological fact'' becomes a claim to erase or misclassify a real biological minority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chromosomal_exhaustiveness_ambiguity, empirical, 'Whether XX/XY chromosomal sex is exhaustive as a biological boundary or whether intersex conditions are frequent enough to render the binary inadequate').

omega_variable(
    reproductive_anatomy_vs_chromosomal_conflict,
    'When reproductive anatomy and chromosomal sex diverge (XX male with female-typical anatomy, XY female with female-typical anatomy due to androgen insensitivity or other conditions), which axis determines female category membership in this reading?',
    'Explicit clarification of the reading''s priority order: (1) XX always female regardless of anatomy? (2) Female anatomy always female regardless of chromosomes? (3) Explicit priority ranking? Empirical documentation of how this reading handles the documented cases where the axes diverge.',
    'The reading claims to ground category in XX/XY chromosomes AND reproductive anatomy. When these diverge, the reading is internally contradictory. Resolution determines whether the reading is truly chromosomal-based (and thus must exclude XX males with female anatomy) or anatomically-based (and thus must include XY females with female anatomy). The contradiction reveals that the reading is simplifying biology into a checklist rather than capturing a unified biological category.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reproductive_anatomy_vs_chromosomal_conflict, empirical, 'Priority order when chromosomal sex and reproductive anatomy diverge').

omega_variable(
    institutional_verification_cost_accumulation,
    'What is the actual institutional cost (in staff time, administrative overhead, legal challenge, and enforcement intensity) of maintaining the XX/XY boundary as category criterion across all affected institutions (prisons, shelters, healthcare systems, sports bodies, census systems)?',
    'Institutional audit: document the verification procedures, staff training, and enforcement infrastructure required to maintain chromosome/anatomy verification; measure the actual enforcement rate vs. stated policy; calculate the administrative cost per correctly classified person',
    'If enforcement cost is negligible: the reading''s practical implementation is low-overhead coordination. If enforcement cost is substantial and growing: the reading is extracting institutional labor in service to a boundary that institutions themselves maintain inconsistently. Rising enforcement cost indicates the reading is shifting from coordination to pure extraction, moving the classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_verification_cost_accumulation, empirical, 'Institutional cost of verifying and enforcing XX/XY category boundary across all affected systems').

omega_variable(
    kernel_contest_framing_underdetermination,
    'Does this reading''s core premise (category membership = XX/XY + reproductive anatomy, immutable, binary) logically foreclose the gender_identity_reading''s core premise (category membership = self-identification, independent of biology), or do the two readings coexist as live positions held by different parties without logical resolution?',
    'Formal analysis of the reading''s axioms: does holding the sex_biology_reading''s foundational claims require denying the truth of the gender_identity_reading''s foundational claims? Or can both be held simultaneously, with disagreement about which axis takes priority in particular contexts? Examination of actual parties holding each reading: do they understand themselves as logically incompatible, or as emphasizing different criteria in different contexts?',
    'If forecloses: the readings are genuinely contradictory, and one must be false. If coexists_with: the readings are emphasizing different axes of a complex phenomenon, and the contest is about priorities and contexts, not truth-falsity. This omega routes the fundamental nature of the kernel contest — whether it is a logical dispute or a practical prioritization dispute — through the framework''s existing resolution apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing_underdetermination, conceptual, 'Whether sex_biology_reading logically forecloses gender_identity_reading or both readings coexist').

omega_variable(
    safety_efficacy_and_specificity_slippage,
    'Does the XX/XY criterion actually predict safety outcomes (reduced sexual violence, successful threat assessment, appropriate care) in the contexts (prisons, shelters, intimate care) where it is invoked, or is the criterion functioning as a proxy that conflates biological sex with risk factors (prior socialization in male-dominated environments, training in physical violence, institutional history of perpetrating sexual assault) that are actually independent of chromosomal sex?',
    'Comparative outcome analysis: incident rates in sex-segregated facilities that use chromosomal sex criteria vs. those using risk assessment, socialization history, or institutional history; epidemiological documentation of whether trans women in female prisons have actual elevated rates of perpetrating sexual assault compared to natal females with similar institutional histories; analysis of whether chromosomal XX in a person raised in male-dominated violent culture and trained in physical assault predicts lower risk than XY in a person raised in female-protective culture',
    'If XX/XY is an effective predictor: the reading genuinely coordinates safety. If the criterion is a proxy confounding biology with socialization/history: the reading is extracting institutional simplicity at the cost of misallocating safety resources (treating chromosomally XX persons with violent histories as safe, treating XY persons without such histories as threats). Slippage converts the reading from tangled_rope (mixed coordination + extraction) toward snare (extraction hiding behind coordination narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_efficacy_and_specificity_slippage, empirical, 'Whether XX/XY criterion predicts safety outcomes or conflates biology with socialization/institutional history').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfb_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sfb_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(sfb_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(sfb_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sfb_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(sfb_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sfb_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sfb_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(sfb_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The woman_female_category kernel decomposes into three constraint stories, one per reading. Each reading has distinct ε values reflecting different structural properties: sex_biology_reading (ε=0.58, tangled_rope) treats category as biological, requires enforcement, produces exclusion; gender_identity_reading (ε values TBD by separate story) treats category as identity, produces different beneficiary/victim sets; hybrid_contextual_reading (ε values TBD by separate story) uses context-specific criteria, produces complexity costs. All three are linked via network.affects_constraints indicating the kernel contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
