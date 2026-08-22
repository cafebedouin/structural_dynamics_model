% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Texts
 *   domain: religious_studies/legal_history/social_stratification
 *
 * SUMMARY:
 *   This story instantiates the colonial-orientalist reading of the
 *   Vedic/Dharmashastra kernel: the claim that these texts constitute (or can
 *   be treated administratively as if they constitute) a unified, timeless
 *   'Hindu law' fit for codification and court enforcement. This is a
 *   distinct constraint from the orthodox reading (which holds Varna
 *   hierarchy is itself divinely commanded, addressed in a sibling story) and
 *   from the reformist reading (which holds the texts are non-prescriptive
 *   spiritual metaphor, also a sibling story). Here the object under scrutiny
 *   is specifically the 18th-19th century British colonial administrative
 *   project — commissioning Sanskrit pandits and Orientalist scholars to
 *   translate and systematize Dharmashastra texts (especially
 *   Manusmriti-derived material) into a single codifiable body of 'Hindu law'
 *   for use in colonial courts, especially via figures and processes
 *   associated with the Anglo-Hindu law tradition. The coordination function
 *   (administrative legibility, reduced adjudication cost) is real; the
 *   extraction runs through selecting one narrow, Brahminical-textual strand
 *   as 'the' law and enforcing it uniformly on a population whose actual
 *   practice was far more regionally and socially heterogeneous. This story
 *   treats codification as scaffold: it was built to solve a specific
 *   administrative-governance problem of an empire administering an
 *   unfamiliar, heterogeneous legal landscape, not as an eternal or necessary
 *   arrangement — hence has_sunset_clause is authored true even though,
 *   empirically, its statutory residue long outlived the colonial
 *   administration that built it (that persistence-past-sunset is itself part
 *   of the mandatrophy story).
 *
 * KEY AGENTS:
 *   - colonial_administration: primary agenda-setter and structural beneficiary (institutional/arbitrage) — commissions and enforces the codification
 *   - orientalist_pandits_and_translators: co-beneficiary and technical agenda-setter (moderate/constrained) — supplies the textual raw material and interpretive authority
 *   - upper_caste_intermediary_elites: secondary beneficiary (organized/mobile) — has customary privilege fixed into enforceable statute
 *   - colonized_legal_subjects_under_codified_caste_law, lower_caste_and_outcaste_communities, women_under_codified_personal_law: primary targets (powerless/trapped) — bear the crystallization of previously fluid disadvantage into fixed law
 *   - regionally_diverse_dharmic_practitioners: excluded voice — their non-Sanskritic, non-textual practice is erased from the codified standard
 *   - postcolonial_legal_historians: analytical observer — documents the administrative construction against the claimed antiquity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.62).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Texts").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/legal_history/social_stratification").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'c7d6dbc0-685b-480a-9707-424c7e3da087').
narrative_ontology:cs_kernel_codification('c7d6dbc0-685b-480a-9707-424c7e3da087', fixed_text).
narrative_ontology:cs_authority_grounding('c7d6dbc0-685b-480a-9707-424c7e3da087', extraction).
narrative_ontology:cs_interpretation_layer_present('c7d6dbc0-685b-480a-9707-424c7e3da087').
narrative_ontology:cs_reading_relation('c7d6dbc0-685b-480a-9707-424c7e3da087', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('c7d6dbc0-685b-480a-9707-424c7e3da087', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_axiom('c7d6dbc0-685b-480a-9707-424c7e3da087', foundational, textual_corpus_admits_singular_administrable_codification).
narrative_ontology:cs_axiom_status(textual_corpus_admits_singular_administrable_codification, holdable).
narrative_ontology:cs_axiom_grounding('c7d6dbc0-685b-480a-9707-424c7e3da087', textual_corpus_admits_singular_administrable_codification, conventional).
narrative_ontology:cs_axiom('c7d6dbc0-685b-480a-9707-424c7e3da087', secondary, colonial_administrative_necessity_justifies_selective_canonization).
narrative_ontology:cs_axiom_status(colonial_administrative_necessity_justifies_selective_canonization, overridden).
narrative_ontology:cs_axiom_grounding('c7d6dbc0-685b-480a-9707-424c7e3da087', colonial_administrative_necessity_justifies_selective_canonization, instrumental).
narrative_ontology:cs_reference_frame('c7d6dbc0-685b-480a-9707-424c7e3da087', precolonial_heterogeneous_customary_practice).
narrative_ontology:cs_drift_state('c7d6dbc0-685b-480a-9707-424c7e3da087', postcolonial_statutory_inheritance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c7d6dbc0-685b-480a-9707-424c7e3da087', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_pandits_and_translators).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_intermediary_elites).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects_under_codified_caste_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_and_outcaste_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, regionally_diverse_dharmic_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commissions the compilation of Dharmashastra texts into a single administrable code (e.g. the Manusmriti-derived codes used in colonial courts) to govern personal law for millions of subjects without needing to understand or adjudicate the actual diversity of local custom. Chooses which texts, which commentators, and which readings become authoritative, then enforces those choices through the court system it controls. Frames the codification as scholarly recovery of an ancient, unified legal tradition rather than as an administrative expedient it is constructing.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Brahmin scholars and European Orientalist scholars employed by the colonial state to translate and systematize Sanskrit legal texts. Gain patronage, prestige, and institutional position by supplying the administration with a coherent, textually-grounded 'Hindu law' that privileges Sanskritic, Brahminical textual authority over regional, oral, and customary legal traditions they are less equipped or willing to represent.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_pandits_and_translators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_pandits_and_translators, agenda_setter).

% Benefit from the codified system because it fixes their customary social and ritual privileges into enforceable colonial law, giving them a durable legal instrument (backed by the colonial court) to assert claims over land, inheritance, and status that were previously contestable through local negotiation and shifting custom.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_intermediary_elites, beneficiary,
    organized, generational, mobile, national).

% Ordinary subjects whose personal law (marriage, inheritance, adoption, property) is now adjudicated according to a single codified text-derived standard rather than the flexible, regionally varying customs they actually lived under. Have no forum to contest the text chosen as authoritative; courts treat the codification as ancient and immutable rather than as a recent administrative construction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects_under_codified_caste_law, payer,
    powerless, biographical, trapped, continental).

% Bear the sharpest costs: codification freezes into formal colonial law hierarchical disabilities that had previously been locally contested, regionally uneven, or informally negotiable, converting fluid social friction into fixed statutory disadvantage enforceable by the colonial court and later inherited by the postcolonial state.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_and_outcaste_communities, payer,
    powerless, generational, trapped, continental).

% Lose customary rights (in inheritance, divorce, remarriage) that varied by region and community once a single Brahminical-textual standard is selected as 'the' law and enforced uniformly; some customary protections available under local practice are erased by the codified standard's more restrictive prescriptions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law, payer,
    powerless, generational, trapped, continental).

% Communities whose actual lived dharma was oral, regionally specific, and administered by local assemblies or caste councils are excluded from the codification process entirely; their practices are either ignored or subsumed under a generalized Sanskritic template that does not reflect them, with no channel to register objection to the choice of authoritative text.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, regionally_diverse_dharmic_practitioners, excluded,
    powerless, biographical, trapped, regional).

% Study the archival record of how colonial administrators selected, commissioned, and enforced particular textual traditions as 'Hindu law,' documenting the gap between the claimed antiquity/timelessness of the codified law and its actual administrative construction in the 18th-19th centuries.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, postcolonial_legal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the colonial administration with a single, textually-anchored, court-enforceable standard for adjudicating personal law across an enormous and locally heterogeneous population, reducing the administrative cost of governing without needing to investigate or accommodate actual local custom in each case.
% TRANSFER_FUNCTION: Moves legal authority away from local, customary, often more flexible or locally negotiated dispute-resolution mechanisms and concentrates it in colonial courts applying a fixed, Brahminical-textual standard — transferring interpretive power from diverse local communities (and often from women and lower castes within them) to colonial administrators and the upper-caste, textually literate intermediaries who supply and interpret the code.
% ABSENT_VOICES: Regionally diverse practitioners of custom, lower-caste and outcaste communities whose practices were locally contested rather than textually fixed, and women whose customary protections varied by region, are not present in the commissioning or translation process; the selection of which Sanskrit texts count as authoritative 'law' is made entirely by administrators and the pandits they employ.
% DISAPPEARANCE_RATIONALE: If the codified 'Hindu law' apparatus vanished, personal law disputes would revert to a much more heterogeneous, locally-administered set of customary practices and forums; the fixed caste and gender disabilities currently backed by colonial-derived statute would lose their state-enforced textual anchor, and courts would need an entirely different (and far more locally variable) basis for adjudication — a substantial legal and administrative rearrangement, not a return to an unchanged status quo, since colonial codification is itself what displaced the prior heterogeneous arrangement.
% FOUNDING_PROBLEM: The colonial administration needed a legible, uniform, textually-citable body of 'native law' it could apply in its courts without the expense and unpredictability of investigating actual local custom case by case across a vast and religiously/regionally diverse population.
% FOUNDING_PROBLEM_CORROBORATION: Postcolonial legal historians (e.g. work tracing the Anglo-Hindu law tradition) attest, from outside the administrative and Brahminical-intermediary beneficiary set, that the administrative need for a single citable code was a colonial-era governance convenience, not a discovery of pre-existing unified law, and that the codified system has persisted as inherited statute long after the colonial administrative rationale that produced it ended; contemporary personal-law reform advocates similarly attest the codified 'ancient' character of the law is a 19th-century construction, not a genealogical fact.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) reflecting the scaffold's genuine administrative coordination function alongside its real transfer of interpretive power and material disadvantage onto excluded and powerless groups; this is not maximal extraction because the codification did provide a functioning (if crude) common law framework rather than pure predation. Suppression (0.62) captures the court system's enforcement of the codified standard against contestation, and the exclusion of alternative customary law from legal standing. Theater ratio rises over the interval (0.2 to 0.4) as the scholarly/administrative rationale ('recovering ancient law') increasingly diverges from the code's actual function as a governance instrument that persisted well past the administrative crisis that motivated it. Accessibility collapse (0.6) and resistance (0.55) are both moderate-high: alternatives (customary, regionally varying law) did not vanish overnight but were steadily displaced from legal standing, and affected communities did contest the codified standard's legitimacy (both during the colonial period and in postcolonial law reform movements), consistent with a constructed, contested scaffold rather than an uncontested mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration derives d near the full-beneficiary end: it authored the selection criteria, funded the translation project, and enforces the resulting code through courts it controls, while bearing none of the substantive costs of misrecognition. Orientalist pandits and upper-caste intermediaries sit closer to the beneficiary end but with less institutional command — their exit options are constrained by dependence on colonial patronage even as they gain durable legal standing for their customary privileges. Colonized legal subjects, lower-caste communities, and women are trapped: courts treat the codified text as the only legally cognizable source of 'Hindu law,' foreclosing the local, customary channels they previously used, which is why their d sits near the full-target end despite the code's genuine (if secondary) administrative benefit to them (predictability, at least, over pure arbitrariness).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative legibility for a colonial court system) is genuinely dead — no colonial administration currently exists to be served by this codification — yet the codified 'Hindu law' framework and its caste/gender-inflected statutory residue persisted into postcolonial personal law long after the administrative rationale disappeared. This dead-founding-problem-plus-persisting-arrangement pattern is exactly what the R5 mismatch check is designed to catch: founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals a zombie/capture pattern worth flagging rather than accepting the code's own claimed timelessness at face value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invented_tradition_vs_genuine_continuity,
    'To what extent did colonial codification invent a novel, artificially unified ''Hindu law'' versus formalize (with distortion) a legal tradition that already had significant textual continuity and cross-regional authority before colonial contact?',
    'Comparative textual-historical analysis of pre-colonial Dharmashastra commentarial traditions (e.g. regional nibandha literature) against the specific texts and interpretations privileged by colonial codifiers, assessing how much pre-colonial cross-regional textual authority already existed versus how much was constructed or selectively amplified by the colonial process.',
    'If codification mostly formalized genuine pre-existing cross-regional textual authority, this reading''s extraction estimate should fall (closer to a rope with modest distortion); if it mostly invented unity from genuinely disparate and locally contested materials, the extraction and suppression estimates authored here understate the construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invented_tradition_vs_genuine_continuity, empirical, 'Degree of colonial invention versus genuine pre-colonial textual continuity in ''Hindu law.''').

omega_variable(
    beneficiary_capture_of_intermediary_elites,
    'Were the upper-caste intermediary elites and Brahmin pandits genuine co-architects of the codification who captured real gains, or were they themselves constrained collaborators with limited alternative livelihoods under colonial patronage structures?',
    'Biographical and institutional-employment analysis of the pandits and scholars involved in the major codification projects, examining their alternative income/status options outside colonial service.',
    'If intermediary elites had meaningful alternatives and profited disproportionately, their beneficiary classification is robust; if they were themselves economically constrained by colonial patronage monopolies, their directionality should sit closer to symmetric than pure beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_intermediary_elites, empirical, 'Whether intermediary elite ''beneficiary'' status reflects genuine capture or constrained collaboration.').

omega_variable(
    kernel_framing_choice_administrative_vs_epistemic,
    'Is the more decision-relevant framing of this kernel the administrative-governance framing used here (why the British state needed a code) or an epistemic framing (how the very category ''Hindu law'' as a bounded, comparable religious-legal system was itself a product of European comparative-religion categories being projected onto South Asian textual practice)?',
    'Cross-check against historiography of the category formation of ''Hinduism'' as a comparative-religion construct (distinct from, but entangled with, the legal-codification history addressed here); if the epistemic framing yields a substantially different beneficiary set (comparative religion scholars, colonial epistemic authority generally) or different ε, that supports treating it as a further sibling constraint rather than folding it into this administrative-governance story.',
    'If the epistemic-category framing is adopted instead, the beneficiary set would extend to comparative-religion scholarship generally and the victim set would include the erasure of non-legal, non-textual religious plurality — a materially different constraint requiring its own story, not a different measurement of this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_administrative_vs_epistemic, conceptual, 'Whether the administrative-governance framing adopted here or an epistemic-category framing is the more decision-relevant cut of this reading, and whether they should be further decomposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 160, 0.57).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(vedi_su_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(vedi_su_t160, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 160, 0.61).
narrative_ontology:measurement(vedi_su_t200, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings reading the same vedic_corpus_social_prescription kernel. colonial_orientalist_reading (this story) treats the constraint as a 19th-century administrative codification project with colonial administration as primary beneficiary and colonized legal subjects as primary victims — a moderate-epsilon scaffold. orthodox_varna_reading treats the constraint as the texts' own direct prescriptive command of a divinely-mandated Varna hierarchy, with different beneficiaries (those occupying favored Varna positions) and a different, likely higher, suppression profile grounded in claimed cosmic/religious sanction rather than colonial administrative convenience. reformist_spiritual_reading denies prescriptive social content entirely, yielding near-zero extraction for the texts themselves (though it may still find extraction in later institutional uses). The three stories share the same underlying textual corpus as their nominal subject but instantiate structurally distinct constraints with distinct ε values, beneficiary/victim sets, and persistence mechanisms — exactly the situation the ε-invariance principle requires resolving by decomposition rather than by parameterizing one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
