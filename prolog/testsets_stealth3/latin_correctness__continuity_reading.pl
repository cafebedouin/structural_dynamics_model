% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Organic Continuation of Classical Latin (Continuity Reading)
 *   domain: historical linguistics/intellectual history/philology
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the contested kernel
 *   latin_correctness: the claim that medieval Latin is the legitimate
 *   continuation of classical Latin through organic linguistic change, such
 *   that vernacular-influenced phonology, expanded vocabulary, and simplified
 *   syntax are natural evolution rather than corruption, and medieval users
 *   are legitimate inheritors rather than failed imitators. The standing
 *   arrangement under contest is the normative standard by which the
 *   correctness of post-classical Latin usage is adjudicated; epsilon is
 *   authored for THAT arrangement as this reading sees it (low extraction, no
 *   victim set), never for the rupture reading's rival arrangement. This is
 *   one member of a three-story constraint family: the rupture reading (fixed
 *   textual standard, medieval usage as corruption, high extraction with
 *   scribes as victims) and the hybrid reading (domain-partitioned norms,
 *   intermediate extraction) instantiate different constraints with
 *   materially different epsilon values and victim structures; all three are
 *   linked via network.affects_constraints and the decomposition is
 *   documented in network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - medieval_scribes_and_authors: Primary beneficiary (organized/identity_locked) - the historical producers of the corpus whose usage the reading legitimizes
 *   - medievalist_philologists: Primary beneficiary (institutional/identity_locked) - modern scholars whose interpretive authority rests on reading medieval texts as Latin on their own terms
 *   - critical_editors_of_medieval_texts: Secondary beneficiary (moderate/mobile) - decide which forms editions transmit and which they emend
 *   - medieval_latin_lexicographers: Agenda setter with beneficiary secondary role (institutional/constrained) - administer the standard through reference works, not coercion
 *   - humanist_classicists: Excluded dissenter (powerful/mobile) - hold the rival fixed-standard reading and contest every legitimizing decision from outside the governing venues
 *   - comparative_linguistic_historians: Analytical observer (analytical/analytical) - trace Latin into the Romance languages and assess the continuity claim from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.1).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.08).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Organic Continuation of Classical Latin (Continuity Reading)").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical linguistics/intellectual history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '840ce633-6237-49f2-9cbd-5dd800484e18').
narrative_ontology:cs_kernel_codification('840ce633-6237-49f2-9cbd-5dd800484e18', distributed).
narrative_ontology:cs_authority_grounding('840ce633-6237-49f2-9cbd-5dd800484e18', expertise).
narrative_ontology:cs_interpretation_layer_present('840ce633-6237-49f2-9cbd-5dd800484e18').
narrative_ontology:cs_reading_relation('840ce633-6237-49f2-9cbd-5dd800484e18', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('840ce633-6237-49f2-9cbd-5dd800484e18', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('840ce633-6237-49f2-9cbd-5dd800484e18', foundational, organic_transmission_preserves_legitimacy).
narrative_ontology:cs_axiom_status(organic_transmission_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('840ce633-6237-49f2-9cbd-5dd800484e18', organic_transmission_preserves_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('840ce633-6237-49f2-9cbd-5dd800484e18', secondary, usage_record_over_reconstruction).
narrative_ontology:cs_axiom_status(usage_record_over_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('840ce633-6237-49f2-9cbd-5dd800484e18', usage_record_over_reconstruction, conventional).
narrative_ontology:cs_reference_frame('840ce633-6237-49f2-9cbd-5dd800484e18', living_transmission_continuum).
narrative_ontology:cs_drift_state('840ce633-6237-49f2-9cbd-5dd800484e18', contemporary_diglossia_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('840ce633-6237-49f2-9cbd-5dd800484e18', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_scribes_and_authors).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medievalist_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, critical_editors_of_medieval_texts).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_latin_lexicographers).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, organic_language_change_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, diachronic_linguistics_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produced the corpus - charters, conciliar acts, liturgy, glosses, scholastic theology, chronicles - in a Latin that absorbed vernacular phonology and coined vocabulary for new institutions. Under this reading their usage counts as legitimate inheritance rather than failed imitation of an ancient model. They could not exit the language without exiting the monastic, chancery, and clerical life it carried; Latin literacy was constitutive of their institutional identity.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_scribes_and_authors, beneficiary,
    organized, generational, identity_locked, continental).

% Modern scholars in medieval Latin philology, diplomatics, and liturgical studies whose interpretive authority and curricular territory rest on reading medieval texts as Latin on its own terms. The reading underwrites their journals, handbooks, and doctoral training. Exit would mean retraining into classics or vernacular fields and abandoning accumulated specialized competence; the specialization is narrow enough that departure carries heavy career cost.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medievalist_philologists, beneficiary,
    institutional, biographical, identity_locked, global).

% Produce critical editions in which they decide which manuscript forms to transmit and which to emend. The reading licenses reproducing medieval forms faithfully - spelling, syntax, new vocabulary - instead of normalizing texts to classical usage. Individually they are mobile: the same skills transfer to other corpora and periods if the standard changed.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, critical_editors_of_medieval_texts, beneficiary,
    moderate, biographical, mobile, continental).

% Compile the national medieval-Latin dictionaries and the Novum Glossarium tradition of reference works that codify which forms count as attested, legitimate usage. They administer the standard through inclusion and citation decisions rather than coercion; no one is compelled to conform, but every editor consults their verdicts. Multi-generational projects bind institutional careers to the framework's continuance.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_latin_lexicographers, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, medieval_latin_lexicographers, beneficiary).

% Heirs of the Renaissance humanist judgment that post-classical Latinity is corruption. They hold the rival fixed-textual standard, teach and compose according to ancient norms, and would contest every legitimizing decision made under this reading. They sustain an independent classical standard in classics departments, schools, and neo-Latin circles - institutionally present in adjacent venues, largely absent from the medieval-studies venues where this reading governs adjudication.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, humanist_classicists, excluded,
    powerful, civilizational, mobile, global).

% Trace Latin's continuous transformation into the Romance languages and document the diglossia of the medieval West. They assess the continuity claim against evidence from both sides - classical philology and Romance linguistics - without administering the standard, collecting from it, or depending on it professionally.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, comparative_linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives editors, lexicographers, teachers, and readers a single diachronic standard spanning classical and medieval Latin: the corpus can be indexed, glossed, taught, and cited as one continuously developing language, so interpretation coordinates across twelve centuries and across national philological traditions without requiring a break or a second parallel standard.
% TRANSFER_FUNCTION: Moves legitimacy and interpretive authority rather than money: recognition flows from the prestige of the classical canon outward to medieval texts and to the scholars who command the full diachronic range, while philological labor is directed toward documenting and glossing change rather than correcting it. No material transfer is enforced on anyone.
% ABSENT_VOICES: Holders of the rupture reading - classically trained humanists and traditionalist educators - would object that legitimizing medieval forms dismantles the fixed standard that makes Latinity assessable; they are institutionally present in classics departments and neo-Latin circles but largely absent from the medieval-studies venues where this reading governs. The vernacular-literate laity of the medieval period, who lived the diglossia from below, are absent as historical agents and represented only through philological mediation.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, critical editions would re-normalize medieval texts to classical forms, the medieval dictionaries would lose their adjudicating role, curricula would split into classical and 'barbarous' tracks, and roughly a millennium of legal, liturgical, and scientific records would drop in practical authority - the scholarly geography of Latin studies would reorganize around whichever sibling reading took over.
% FOUNDING_PROBLEM: After the humanist condemnation of medieval Latinity as corruption, European scholarship faced a corpus of unprecedented size - charters, councils, liturgy, glosses, science - written in a Latin that violated classical norms, and risked losing practical access to it. The continuity reading was built to solve that access-and-authority problem: to make the medieval corpus readable and citable as Latin without first correcting it into an ancient mold.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Romance-language linguists attest the organic-change substrate by tracing Latin's continuous transformation into French, Iberian, and Italian; historians of law and liturgy attest the practical stakes of corpus access; the surviving insistence of classical education on ancient norms attests that the contest, and therefore the reading's work, is not finished. No party outside these disputes has reason to assert the founding problem, and none disputes that the access problem existed.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.10) because the reading enforces no transfer: its only costs are the ordinary discipline any shared standard imposes and the channeling of philological labor toward documentation rather than correction. Suppression is very low (0.08) and is a raw structural property, unscaled by power or scope: holders of the rupture and hybrid readings publish, teach, and edit freely; nothing is coerced. Theater is low (0.12): congresses, festschriften, and anniversary volumes carry ritual weight, but the lexicographic and editorial core performs real coordinating work. Accessibility_collapse is low (0.18) because the sibling readings remain fully live alternatives - understanding the continuity claim does not close off the fixed-standard or domain-partitioned positions. Resistance is moderate (0.45): sustained classicist and traditionalist-educator dissent from outside, plus internal challenge from diglossia scholarship, against near-consensus inside medieval-studies venues. The claimed type (rope) is stated from structure - a genuine coordination function, net-beneficiary participants, no suppressed exits - while the metrics are stated from description; the engine computes per-seat classifications from the structural data and any divergence from the claim is the measurement the corpus exists to take. The temporal series are deliberately flat with slow drift: this constraint lacks crisis cycles, and its history is gradual consolidation, so no oscillation or intermittent-reinforcement dynamic is asserted. Gain_flow is authored as diffuse after checking every seat: lexicographers administer but collect no rents, philologists gain dispersed professional standing, and with epsilon at 0.10 there is negligible extraction for any seat to capture. Fixing_cost is authored as cheap on its own evidence: the constraint is an interpretive stance, not a machine - if it collapsed, the documentary apparatus (dictionaries, editions, corpora) survives independently and a generation of scholars could re-adopt the standard at low cost, which is why neglect here would be transient rather than entombing.
 *
 * PERSPECTIVAL GAP:
 *   Because every seated party is a beneficiary, the engine should find little divergence in computed extraction across seated positions; the real perspectival gap runs between seated and excluded seats. From inside medieval studies the reading is near-invisible - simply how the field works - and its identity-locked beneficiaries experience it as home: for medievalist_philologists the lock is professional (career path dependence on a specialized competence), and for medieval_scribes_and_authors it was institutional-relational (Latin literacy constituted clerical identity, so exit meant leaving the transregional role the language carried). From the excluded humanist_classicist seat the same claim reads as abdication of the fixed standard that makes Latinity assessable at all. From the observer seat it is one indexical position in a three-way contest over a shared kernel. If the identity frames broke - if medieval Latin were reclassified as a foreign-language tradition rather than an inheritance - the reading's constituency would shrink to interest-held beneficiaries and the contest would intensify sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups derive low directionality (subsidy-side or near-symmetric): the reading legitimizes their practice, authority, or editorial method and extracts almost nothing from them. No victims are declared and no payer seats exist, so no seat sits near the full-target end; the continental-to-global scope modestly amplifies whatever small extraction exists, but the base rate is too low for amplification to matter. The excluded humanist_classicists sit outside the beneficiary/victim derivation entirely: their opposition is positional (they hold a rival reading of the same kernel), not a cost borne under this constraint. Comparative_linguistic_historians hold the analytical seat and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - practical access to and authority for a millennium of records written in non-classical Latin after the humanist condemnation - is still live, so no mandatrophy_resolved declaration is made and none is keyed to any metric. The classification guards against two opposite mislabelings. An extraction-first reading could mistake professional self-interest (philologists protecting curricular territory) for snare signals; the structural data - no victims, no suppressed exits, open contest with live sibling readings - blocks that inference. Conversely, the professional_autonomy_persistence omega prevents the rope verdict from hardening into complacency: if the reading's persistence turned out to be career-protective rather than access-driven, the theater_ratio would be understated and inertial maintenance would need to be taken seriously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_latin_correctness,
    'This story instantiates only the continuity_reading of the latin_correctness kernel; which reading actually governs adjudication in a given venue, and how would the sibling readings restructure this constraint?',
    'Survey which standards critical editions, university curricula, examination regimes, and lexicographic projects actually apply across venues, mapping each venue to continuity, rupture, or hybrid adjudication.',
    'Under the rupture reading the same subject matter computes as high-extraction with medieval scribes and authors as victims (snare or tangled_rope profile); under the hybrid reading extraction is domain-partitioned (tangled_rope profile). This story''s rope verdict holds only for the continuity seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_latin_correctness, conceptual, 'Kernel-level reading indexicality: the constraint''s identity depends on which reading of latin_correctness is instantiated.').

omega_variable(
    transmission_mode_diglossia,
    'Was medieval Latin organically transmitted within a continuous speech community, or acquired as a school-taught second language under vernacular diglossia?',
    'Manuscript evidence of pronunciation practice, records of school programs and classroom Latin use, and diglossia-era scholarship tracing the vernacular/Latin division of labor across the medieval West.',
    'If medieval Latin was L2-acquired under diglossia, the ''organic change'' premise weakens to ''institutionalized second-language drift,'' softening the legitimacy argument and increasing the constructed share of the standard; the rope verdict would rest more on scholarly convention than on natural continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mode_diglossia, empirical, 'Whether the transmission underlying the continuity claim was native-like organic inheritance or schooled second-language acquisition.').

omega_variable(
    professional_autonomy_persistence,
    'Does the continuity reading persist because it solves the corpus-access problem, or because it sustains medieval studies'' professional autonomy from classics departments?',
    'Counterfactual institutional analysis: if classics departments fully absorbed medieval texts and applied their own editorial norms, would adjudication practice in medieval studies actually change, or would the continuity standard be defended irrespective of access outcomes?',
    'If persistence is autonomy-driven, the theater_ratio is understated and the rope verdict drifts toward inertial maintenance; if access-driven, the rope verdict is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(professional_autonomy_persistence, empirical, 'Functional versus career-protective persistence of the continuity standard.').

omega_variable(
    naturality_vs_construction_boundary,
    'How much of the reading''s force comes from the natural fact of language change versus a constructed legitimacy verdict layered on top of it?',
    'Separate the descriptive thesis (linguistic change occurred continuously and is documentable) from the normative thesis (continuous change confers legitimacy) and evaluate each independently against the philological record.',
    'Isolating the constructed layer confirms the constraint as a scholarly coordination convention (rope) rather than a quasi-natural law; conflating the layers risks false-summit treatment in which a contestable normative choice presents as natural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_construction_boundary, conceptual, 'Boundary between the natural process of language change and the constructed legitimacy claim built upon it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_continuity_tr_t1800, latin_correctness__continuity_reading, theater_ratio, 1800, 0.06).
narrative_ontology:measurement_basis(latin_continuity_tr_t1800, observed).
narrative_ontology:measurement(latin_continuity_tr_t1830, latin_correctness__continuity_reading, theater_ratio, 1830, 0.07).
narrative_ontology:measurement_basis(latin_continuity_tr_t1830, observed).
narrative_ontology:measurement(latin_continuity_tr_t1870, latin_correctness__continuity_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement_basis(latin_continuity_tr_t1870, observed).
narrative_ontology:measurement(latin_continuity_tr_t1920, latin_correctness__continuity_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement_basis(latin_continuity_tr_t1920, observed).
narrative_ontology:measurement(latin_continuity_tr_t1970, latin_correctness__continuity_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement_basis(latin_continuity_tr_t1970, observed).
narrative_ontology:measurement(latin_continuity_tr_t2000, latin_correctness__continuity_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement_basis(latin_continuity_tr_t2000, observed).
narrative_ontology:measurement(latin_continuity_tr_t2026, latin_correctness__continuity_reading, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(latin_continuity_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(latin_continuity_be_t1800, latin_correctness__continuity_reading, base_extractiveness, 1800, 0.11).
narrative_ontology:measurement_basis(latin_continuity_be_t1800, observed).
narrative_ontology:measurement(latin_continuity_be_t1830, latin_correctness__continuity_reading, base_extractiveness, 1830, 0.1).
narrative_ontology:measurement_basis(latin_continuity_be_t1830, observed).
narrative_ontology:measurement(latin_continuity_be_t1870, latin_correctness__continuity_reading, base_extractiveness, 1870, 0.09).
narrative_ontology:measurement_basis(latin_continuity_be_t1870, observed).
narrative_ontology:measurement(latin_continuity_be_t1920, latin_correctness__continuity_reading, base_extractiveness, 1920, 0.09).
narrative_ontology:measurement_basis(latin_continuity_be_t1920, observed).
narrative_ontology:measurement(latin_continuity_be_t1970, latin_correctness__continuity_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement_basis(latin_continuity_be_t1970, observed).
narrative_ontology:measurement(latin_continuity_be_t2000, latin_correctness__continuity_reading, base_extractiveness, 2000, 0.11).
narrative_ontology:measurement_basis(latin_continuity_be_t2000, observed).
narrative_ontology:measurement(latin_continuity_be_t2026, latin_correctness__continuity_reading, base_extractiveness, 2026, 0.1).
narrative_ontology:measurement_basis(latin_continuity_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Latin correctness' decomposes into three structurally distinct constraints sharing one kernel. The continuity reading (this file) treats legitimacy as preserved through organic transmission: epsilon ~0.10, no victim set, rope profile. The rupture reading treats classical Latin as a fixed textual standard requiring reconstruction, making medieval usage corruption: high epsilon with medieval scribes and authors as victims, snare-or-tangled-rope profile. The hybrid reading partitions domains - classical norms for literary/rhetorical use, medieval forms legitimate for technical/practical use: intermediate, domain-indexed epsilon, tangled-rope profile. The upstream continuity reading influences the hybrid reading because its documentation of legitimate technical and practical usage created exactly the legitimacy conditions the hybrid reading grants to those domains; continuity and rupture coexist as live positions held by different scholarly factions, with the hybrid reading demonstrating that a single framework can partition rather than choose between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
