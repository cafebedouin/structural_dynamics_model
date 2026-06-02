% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Orthodox Literalist Reading: Eternal Varna/Jati Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The orthodox literalist reading of dharmasastra prescriptions frames the
 *   varna (and embedded jati/caste) hierarchy as eternal, divinely revealed
 *   truth requiring literal observance across generations. This reading
 *   instantiates one pole of a contested kernel spanning at least three
 *   distinct normative commitments: the orthodox literalist (this
 *   constraint), the reformist contextual (dharmasastra applicable only to
 *   specific historical periods and geographic contexts), and the
 *   abolitionist (dharmasastra varna prescriptions are normatively
 *   indefensible and should be repudiated). The literalist reading treats the
 *   dharmasastra corpus (especially Manusmrti, Yajnavalkya-smrti, and their
 *   commentarial tradition) as a stable, internally authoritative kernel
 *   grounding legitimate social order. The constraint classifies as a Snare
 *   from the perspectives of dalits and shudras — trapped by birth-determined
 *   occupational status, ritual prohibition, and soteriological exclusion —
 *   and as a Rope from the brahmin priesthood and upper-caste landholding
 *   class, who experience the varna framework as coordination mechanism
 *   legitimizing their institutional authority and property concentration.
 *   The analytical observer risks reading the constraint as a Mountain
 *   (cosmic natural law), but structural data reveals this as a false summit:
 *   identifiable beneficiaries and enforceable suppression mechanisms
 *   indicate constructed hierarchy, not inherent cosmic order. Over the
 *   measurement interval (400 units, spanning roughly classical to
 *   contemporary period), suppression_requirement has declined from 0.85 to
 *   0.62 as state legal apparatus and constitutional prohibition of
 *   untouchability have eroded enforcement capacity, while theater_ratio has
 *   risen from 0.25 to 0.45 as the constraint shifts from actively enforced
 *   hierarchy to increasingly performative theological affirmation.
 *
 * KEY AGENTS:
 *   - Brahmin Priesthood: Primary beneficiary (institutional/arbitrage) — dharmasastra prescribes monopoly on Vedic knowledge, ritual authority, and spiritual legitimacy; experiences constraint as pure coordination enabling institutional control
 *   - Upper-Caste Landholding Class: Secondary beneficiary (powerful/arbitrage) — varna hierarchy legitimizes property ownership concentration, exempts from service obligation; benefits from caste-tied occupation segregation
 *   - Dalit Community: Primary victim (powerless/trapped) — prescribed to untouchable status, occupational servitude, ritual contamination; zero exit options within literalist framework; bears maximum extraction
 *   - Shudra Householder Class: Secondary victim (moderate/constrained) — restricted to occupational caste and household ritual; excluded from Vedic knowledge and higher soteriological paths; some regional/merchant-class escape routes but constraint prescribes immobility
 *   - Women (Brahmin and Other Varnas): Mixed victim (moderate/constrained) — prescribed to male guardianship, excluded from textual study and public ritual authority; permitted household ritual authority providing some coordination benefit but asymmetric extraction via legal dependency
 *   - Field of Soteriological Alternatives: Abstract victim — dharmasastra hierarchy restricts legitimate salvation pathways; non-literalist alternatives (bhakti, advaita) may or may not provide genuine access (omega variable)
 *   - Contemporary Orthodox Authority Structure: Institutional actor (institutional/arbitrage) — maintains literalist framework through theological affirmation; functionally degraded due to loss of state legal enforcement (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.68).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.72).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.68).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Orthodox Literalist Reading: Eternal Varna/Jati Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '59b9a66b-8f01-442f-ab44-f80ca08c5a9b').
narrative_ontology:cs_kernel_codification('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', fixed_text).
narrative_ontology:cs_authority_grounding('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', lineage).
narrative_ontology:cs_interpretation_layer_present('59b9a66b-8f01-442f-ab44-f80ca08c5a9b').
narrative_ontology:cs_reading_relation('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', foundational, varna_hierarchy_eternally_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternally_revealed, holdable).
narrative_ontology:cs_axiom_grounding('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', varna_hierarchy_eternally_revealed, theological).
narrative_ontology:cs_axiom('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', foundational, brahmin_priesthood_vedic_monopoly_prescribed).
narrative_ontology:cs_axiom_status(brahmin_priesthood_vedic_monopoly_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', brahmin_priesthood_vedic_monopoly_prescribed, deontological).
narrative_ontology:cs_reference_frame('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', eternal_cosmic_order_framework).
narrative_ontology:cs_drift_state('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59b9a66b-8f01-442f-ab44-f80ca08c5a9b', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_landholders).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_excluded_from_ritual).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, field_soteriological_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DALIT COMMUNITY (SNARE) — Trapped by caste hierarchy enforced through ritual prohibition, occupational restriction, and legal sanction. Dharmasastra prescription for shudra/dalit status prescribes untouchability, exclusion from Vedic study, and occupational servitude. Zero exit options within the orthodox framework — birth determines permanent status. Experiences maximum extraction: labor obligation without property rights, ritual contamination, and denial of soteriological access.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: SHUDRA HOUSEHOLDER (SNARE) — Bound to occupational caste; permitted household rites but excluded from Vedic knowledge and higher soteriological paths. High extraction via enforced occupational segregation and educational barrier. Constrained rather than trapped only because regional variation in enforcement and merchant-class mobility offer limited escape routes, but the constraint itself prescribes immobility.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMIN WOMAN (TANGLED ROPE) — Mixed constraint: dharmasastra prescribes ritual authority through household religious functions (coordination benefit) but mandates legal guardianship by male relatives, prohibits independent property ownership, and excludes from textual study and public ritual authority. Extraction is asymmetric but not maximal — household coordination role provides some authority within constrained sphere.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BRAHMIN PRIESTHOOD (ROPE) — Primary beneficiary (institutional/arbitrage). Dharmasastra prescribes brahmin monopoly on Vedic knowledge, ritual performance, and spiritual authority. The constraint functions as pure coordination at this level: establishing shared ritual standards, inheritance of textual knowledge, and collective authority. Extraction flows toward this agent; they experience the constraint as coordination mechanism that legitimizes their institutional position.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: UPPER-CASTE LANDHOLDING CLASS (ROPE) — Secondary beneficiary (powerful/arbitrage). Dharmasastra varna prescriptions legitimize land ownership concentration, exemption from service obligations, and ritual superiority. Functions as coordination mechanism for property transfer and agrarian hierarchy. Beneficiary with full exit options (can redefine observance or migrate) — experiences constraint as enabling coordination, not extraction.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: CONTEMPORARY ORTHODOX AUTHORITY STRUCTURE (PITON) — Institutional actors claiming to enforce classical dharmasastra prescriptions face a degraded functional situation. The constraint's suppression mechanism (ritual prohibition, occupational ban, untouchability) persists through institutional and cultural inertia despite loss of legal enforcement capacity (post-1950 Indian constitutionalism, statutory abolition of untouchability). The authority structure maintains the literalist framework through theological affirmation and community tradition, but the functional extraction mechanism has atrophied — observed as theater ratio (0.45) reflecting maintenance cost without proportional functional return.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a theological universal standpoint, the orthodox literalist reading frames varna hierarchy as an immutable cosmic law (cosmic-order reading of Bhagavad Gita 4.13, Manusmrti prescriptive hierarchies). The constraint appears as an inherent feature of dharmic order itself — natural, unchangeable, transcendent. However, this perspective represents a false summit: beneficiary groups (brahmin priesthood, upper-caste landholders) are identifiable, and the extraction mechanism is enforceable through institutional and social coercion, not logical/physical inevitability. The 'naturalness' claim itself requires active enforcement and theological maintenance.
constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dharmasastra_corpus__orthodox_literalist, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, TR),
    TR >= 0.70.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts labor obligation (shudra/dalit), ritual exclusion (dalit), educational deprivation (all non-brahmin varnas), and soteriological access restriction (all non-brahmin varnas except through subordinate household roles for some women). The extraction is not maximal (0.72+) because some categories (brahmin women, merchant shudras) have partial exits or benefits embedded in the constraint. Suppression (0.72): High. The constraint operates through multiple suppression mechanisms: legal prohibition on shudra/dalit property ownership, religious prohibition on Vedic study, occupational caste restriction, and ritual contamination sanction. Contemporary measurement shows suppression at 0.62 (reduced from historical 0.85) due to constitutional prohibition of untouchability and state legal enforcement against caste discrimination, but community-level enforcement through ritual sanction and occupational segregation persists. Theater ratio (0.45, rising from 0.25): The constraint has shifted from active enforcement (historical period, state apparatus supporting varna-based legal disabilities) to increasingly performative theological affirmation (contemporary period, state apparatus opposing hierarchy, constraint maintained through community tradition and religious authority rather than legal sanction). This rise in theater reflects the transition toward piton classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The brahmin priesthood sees a coordination mechanism enabling their institutional monopoly on legitimate spiritual authority (Rope). The dalit community sees a trap enforcing occupational servitude and soteriological exclusion with no exit (Snare). The reformist reading (sibling constraint) would classify the same structural phenomenon as a historically contingent arrangement that can and should be contextually reinterpreted or abandoned. The abolitionist reading (sibling constraint) would classify it as an extractive normative system that should be categorically repudiated. The contemporary orthodox authority structure sees degraded functionality: the constraint persists through theological affirmation and community tradition despite loss of legal-state enforcement (Piton). The analytical observer risks seeing an immutable cosmic law (Mountain), but the structural data reveals this as a false summit — the 'eternal' quality requires active maintenance, the 'natural' character requires institutional enforcement, the 'revealed' status requires theological affirmation. These are the markers of constructed constraint, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies dramatically across observer positions, which is diagnostic of its status as a contested, constructed hierarchy rather than a natural law. Brahmin priesthood (institutional/arbitrage) derives d ≈ 0.10 (beneficiary with full exit) → f(d) ≈ -0.01 → χ approaching zero or negative. This agent experiences the constraint as pure coordination. Upper-caste landholders (powerful/arbitrage) derive d ≈ 0.20 → f(d) ≈ 0.02 → χ approaching zero. Dalit community (powerless/trapped) derives d ≈ 0.95 → f(d) ≈ 1.42 → χ highly amplified. The magnitude of chi variation across observer positions is diagnostic: if the constraint were a natural law, chi would be invariant across all observers (all would experience the constraint at the same intensity). The fact that chi ranges from near-zero (beneficiaries) to highly amplified (victims) is evidence of constructed extraction, not natural constraint. Spatial scope is continental throughout, so σ(S) = 1.1 uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by instantiating one reading of a contested kernel. The orthodox literalist reading produces a Snare from victim perspectives (dalits, shudras) and a Rope from beneficiary perspectives (brahmin priesthood, upper-caste landholders). There is no mandate-quality ambiguity (confusion about whether coordination or extraction is primary) — the ambiguity is located in the kernel itself: whether dharmasastra hierarchy is an eternal cosmic order or a constructed human normative system. The literalist reading commits to the former, which grounds its claims to immutability and legitimacy. This commitment determines the classification. The reformist and abolitionist readings reject this commitment and produce different classifications (not included in this constraint story — see network.affects_constraints). The mandatrophy between literalist and reformist/abolitionist is resolved not by reanalyzing the same constraint from different perspectives but by recognizing that each reading instantiates a *different* constraint with a different epsilon value. The literalist reading (this story) has ε ≈ 0.68 because it accepts the beneficiary/victim asymmetry as prescribed by eternal cosmic law. The reformist reading would have ε ≈ 0.55 because it contextualizes the asymmetry as historically contingent. The abolitionist reading would have ε ≈ 0.75+ because it treats the hierarchy as purely extractive without coordination function. The constraints form a family (network linked), not a single constraint viewed from multiple angles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternal_revelation_vs_contingent_composition,
    'Is dharmasastra hierarchy eternally revealed truth (apauruseya/divinely transmitted) or contingently composed human legal tradition (pauru­seya/human-authored)?',
    'Textual-historical analysis: dating of dharmasastra corpus layers, evidence of compositional revision and adaptation to historical circumstances, comparison with contemporaneous non-Vedic legal traditions showing convergent problem-solving rather than unique revelation. Liturgical-archaeological evidence: ritual practice documentation across regions and centuries showing diversity and drift inconsistent with stable eternal prescription.',
    'If eternal revelation: constraint approaches mountain classification (unchangeable cosmic law). If contingent composition: constraint is human-authored normative arrangement (snare with identifiable beneficiaries). This distinction is foundational to whether the orthodox literalist reading forecloses or coexists with reformist/abolitionist readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternal_revelation_vs_contingent_composition, empirical, 'Whether dharmasastra hierarchy is eternal revelation or contingent human composition').

omega_variable(
    jati_enforcement_mechanisms_contemporary,
    'What proportion of current caste hierarchy enforcement operates through institutional legal sanction versus community/cultural/religious enforcement?',
    'Legal-institutional audit: statutory protections (SC/ST Act, constitutional prohibition on untouchability) versus actual enforcement capacity and incidence. Community-level ethnographic documentation: proportion of caste enforcement through ritual sanction, marriage restriction, occupation segregation, and violence versus state legal apparatus. Temporal trend analysis: enforcement capacity pre-1950 vs post-1950.',
    'If state legal enforcement substantial: piton classification may be premature — active suppression mechanism still operational. If community/religious enforcement dominant and state apparatus disabled: piton classification confirmed — constraint persists through institutional inertia and theological affirmation despite legal prohibition. Affects measurement of suppression_requirement trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jati_enforcement_mechanisms_contemporary, empirical, 'Contemporary enforcement mechanisms for caste hierarchy (legal-institutional vs community-cultural)').

omega_variable(
    brahmin_priesthood_dependence_on_hierarchy,
    'What proportion of brahmin institutional authority depends on the varna hierarchy itself versus on accumulated property, educational advantage, and social networks independent of dharmasastra prescription?',
    'Comparative analysis: brahmin communities in reform-tradition regions (Karnataka, Maharashtra) where dharmasastra literalism has been contested for 150+ years; measurement of brahmin institutional authority and property control in these regions versus orthodox heartlands. Analysis of brahmin institutional adaptation to secular constitutional frameworks and loss of ritual monopoly — institutional survival despite erosion of literalist dharmasastra authority.',
    'If authority highly dependent on varna prescription: beneficiary group faces existential threat from reformist/abolitionist readings (relation to sibling reading shifts toward foreclosure). If authority substantially independent of caste hierarchy: beneficiary group can survive and potentially benefit from reform (relation shifts toward coexistence). Affects whether brahmin priesthood would foreclose or coexist-with reformist reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmin_priesthood_dependence_on_hierarchy, empirical, 'Degree of brahmin institutional authority dependence on varna hierarchy').

omega_variable(
    soteriological_alternative_accessibility,
    'Do non-literalist dharma traditions (bhakti, advaita, tantric, local goddess worship) provide genuine soteriological alternatives to varna-hierarchy-dependent Vedic ritual access, or do they reproduce the hierarchy in different idioms?',
    'Textual analysis of bhakti and advaita soteriological claims (Ramanuja, Kabir, Chaitanya emphasis on devotion over varna-based ritual); ethnographic documentation of actual initiation and teaching patterns in bhakti lineages and advaitin monasteries (do they admit all varnas equally?); comparison of dalit membership in bhakti lineages versus varna-segregated orthodox ritual communities.',
    'If alternatives genuinely accessible: field_soteriological_alternatives may not be a true victim (alternative exit routes exist); snare classification may be overstated. If alternatives reproduce hierarchy or are unavailable to dalits: field_soteriological_alternatives is correctly identified as victim; snare classification confirmed. Affects whether the constraint''s scope of extraction is truly comprehensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soteriological_alternative_accessibility, empirical, 'Whether non-literalist Hindu traditions provide genuine soteriological alternatives to varna hierarchy').

omega_variable(
    committer_frame_kernel_stability,
    'Is the dharmasastra corpus itself a stable, internally consistent kernel, or does it contain irreducible internal contradictions that different readings (literalist, reformist, abolitionist) exploit selectively?',
    'Comparative textual analysis of Manusmrti, Yajnavalkya-smrti, Narada-smrti, Parashara-smrti on varna duties, mobility, and soteriological access. Identification of internal contradictions (e.g., some texts permitting varna mobility under specified conditions vs others prescribing immobility). Analysis of whether literalist reading selects consistent subset or reconciles contradictions through interpretive hierarchy (e.g., privileging certain texts as authoritative).',
    'If genuinely consistent kernel: readings are truly different *interpretations* of a single stable text, supporting coexists_with framing. If kernel is internally contradictory: readings are selective activations of kernel''s multiple logics, raising question whether there is one kernel or multiple embedded kernels. This affects the metaphysical status of the reading_relations (are they interpretive variants or are they competing extractions from heterogeneous source material?).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_stability, conceptual, 'Whether dharmasastra corpus is internally consistent or contains contradictions readings exploit selectively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dh_lit_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dh_lit_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.35).
narrative_ontology:measurement(dh_lit_tr_t400, dharmasastra_corpus__orthodox_literalist, theater_ratio, 400, 0.45).

% Extraction over time
narrative_ontology:measurement(dh_lit_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(dh_lit_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.68).
narrative_ontology:measurement(dh_lit_be_t400, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dh_lit_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dh_lit_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(dh_lit_su_t400, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% The dharmasastra corpus is a contested kernel instantiated by at least three structurally distinct constraints: orthodox_literalist (ε=0.68, Snare, this story), reformist_contextual (ε=0.55, Tangled Rope, contextual reinterpretation allowing regional/historical variation), and abolitionist_rejection (ε=0.75+, Snare, wholesale repudiation of varna hierarchy). Each reading activates different aspects of the textual tradition and produces different victim sets, beneficiary groups, and enforcement mechanisms. The literalist reading treats the corpus as stable, internally consistent, eternally authoritative; reformist reading treats it as contingently composed, context-dependent; abolitionist reading treats it as normatively indefensible. The network topology reflects logical influence: literalist reading forecloses or coexists-with sibling readings depending on empirical resolution of whether dharmasastra hierarchy is eternal revelation or contingent composition (omega variable: eternal_revelation_vs_contingent_composition). If revelation is established, literalist forecloses others; if composition is established, literalist coexists-with alternatives as competing readings of a heterogeneous textual tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
