% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Colonial Orientalist Codification of 'Hindu Law'
 *   domain: religious/legal/colonial-administrative
 *
 * SUMMARY:
 *   In 1772 the Bengal administration resolved that family, caste, and
 *   inheritance disputes would be decided by 'the laws of the Shaster',
 *   administered first through pandit advisers and then, as those proved
 *   inconvenient, through a fixed textual corpus recovered by Orientalist
 *   scholarship — Jones's translations, the Gentoo Code, and ultimately the
 *   Anglo-Hindu case-law edifice. The reading instantiated here treats the
 *   Vedic/Dharmashastra corpus as a single, timeless, positive-law code that
 *   scholarship can recover and the colonial state can enforce; the
 *   arrangement built on that reading ran from the Hastings Plan to the Hindu
 *   Code Bills (1955-56), which deliberately dismantled it. EPSILON REFERENT:
 *   the standing arrangement under contest is the colonial codification
 *   regime itself, assessed by this reading's own lights — the reading
 *   presents the arrangement as legitimate recovery and administration, so
 *   its ε is moderate rather than maximal, even though the structural
 *   beneficiary/victim data below record substantial asymmetric transfer. Per
 *   the committer-frame rules, this file instantiates ONLY the
 *   colonial_orientalist_reading; the orthodox_varna_reading and
 *   reformist_spiritual_reading are separate constraints (separate files,
 *   separate ε, separate victim sets), linked via
 *   network.affects_constraints, and nothing here averages across them.
 *   CLAIM/METRIC INDEPENDENCE: claimed_type is scaffold because the
 *   arrangement's own justification was transitional stewardship (the
 *   civilizing-mission framing carried an implicit sunset, honored in the
 *   breach for a century and finally executed in 1947-56); the metrics are
 *   authored descriptively from the historical record, and where the engine's
 *   per-seat computations diverge from the claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - colonial_administration: agenda-setter (institutional/arbitrage) — authors, administers, and enforces the codified corpus; the seat the arrangement's gains accrue to
 *   - - company_revenue_establishment: beneficiary (institutional/arbitrage) — collects revenue through the fixed-category machinery
 *   - - colonial_legal_profession: beneficiary (organized/mobile) — livelihood constituted by the codified corpus and its case law
 *   - - upper_caste_landholding_elites: dual-positioned beneficiary/payer (organized/constrained) — property secured by codified family law, political autonomy surrendered to it
 *   - - colonized_hindu_subjects: primary target (powerless/trapped) — compulsory jurisdiction, no territorial exit
 *   - - lower_caste_communities: differential target (powerless/trapped) — mobility foreclosed by fixed legal category
 *   - - hindu_women_under_codified_family_law: differential target (powerless/trapped) — family status fixed by state record
 *   - - traditional_pandit_interpreters: displaced intermediary (moderate/constrained) — early co-optation, later displacement
 *   - - indian_nationalist_reformers: excluded voice (organized/constrained) — objectors outside the code-making room
 *   - - postcolonial_historians: analytical observer (analytical/analytical) — sees the full structure from after its end
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.48).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.6).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Codification of 'Hindu Law'").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious/legal/colonial-administrative").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '3313c0d4-47fc-4720-9058-816cd106ffad').
narrative_ontology:cs_kernel_codification('3313c0d4-47fc-4720-9058-816cd106ffad', fixed_text).
narrative_ontology:cs_authority_grounding('3313c0d4-47fc-4720-9058-816cd106ffad', extraction).
narrative_ontology:cs_interpretation_layer_present('3313c0d4-47fc-4720-9058-816cd106ffad').
narrative_ontology:cs_reading_relation('3313c0d4-47fc-4720-9058-816cd106ffad', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('3313c0d4-47fc-4720-9058-816cd106ffad', vedic_corpus_social_prescription__reformist_spiritual_reading, influences).
narrative_ontology:cs_axiom('3313c0d4-47fc-4720-9058-816cd106ffad', foundational, corpus_constitutes_justiciable_positive_code).
narrative_ontology:cs_axiom_status(corpus_constitutes_justiciable_positive_code, holdable).
narrative_ontology:cs_axiom_grounding('3313c0d4-47fc-4720-9058-816cd106ffad', corpus_constitutes_justiciable_positive_code, empirically_contingent).
narrative_ontology:cs_axiom('3313c0d4-47fc-4720-9058-816cd106ffad', foundational, textual_recovery_surpasses_native_practice).
narrative_ontology:cs_axiom_status(textual_recovery_surpasses_native_practice, overridden).
narrative_ontology:cs_axiom_grounding('3313c0d4-47fc-4720-9058-816cd106ffad', textual_recovery_surpasses_native_practice, instrumental).
narrative_ontology:cs_reference_frame('3313c0d4-47fc-4720-9058-816cd106ffad', timeless_unified_scriptural_law_code).
narrative_ontology:cs_drift_state('3313c0d4-47fc-4720-9058-816cd106ffad', postcolonial_philological_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('3313c0d4-47fc-4720-9058-816cd106ffad', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, company_revenue_establishment).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_legal_profession).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_hindu_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_under_codified_family_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_pandit_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_pandit_interpreters).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the courts, census offices, and revenue machinery the codified law operates through. Writes the codes, appoints judges, decides which texts count as authoritative and which customs as exceptions. Collects land revenue and court fees through the same machinery. Can amend, reinterpret, or extend the codes at will; its own exposure to the rules it administers is minimal.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Assesses and collects land tax across the presidencies. Fixed, text-backed categories of tenure and household make assessment predictable and appeals routable. Its officers rotate on short postings; few spend their careers living under the rules they administer.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, company_revenue_establishment, beneficiary,
    institutional, biographical, arbitrage, continental).

% Judges, barristers, and pleaders trained in the codified system. The fixed corpus generates the case law, commentaries, and pleadings their livelihood consists of, and it includes a growing body of Indian lawyers who master the system and rise within it. An individual member can leave practice or change specialty; the profession as a whole is bound to the corpus's continued operation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_legal_profession, beneficiary,
    organized, biographical, mobile, continental).

% Landholding families whose succession, partition, and property rights the codified family law secures along scriptural lines favorable to their holdings. The same codification subordinates them politically to the administration and converts their ritual precedence into a fixed, administrable entry. Leaving the arrangement would mean forfeiting the property order that secures their position.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_landholding_elites, payer).

% Everyone subject to the codified law in family, caste, and succession matters. Jurisdiction is compulsory: disputes route to colonial courts applying the fixed corpus, and local or customary forums progressively lose recognition. Census and court records assign identity entries from the codified categories. There is no jurisdictional exit; subjecthood is territorial and compulsory.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_hindu_subjects, payer,
    powerless, generational, trapped, continental).

% Communities whose service obligations, ritual status, and possibilities of mobility the codified hierarchy records and enforces as settled legal fact. Where earlier practice left room for negotiation, migration, or reidentification through sectarian affiliation, the codified category follows the person into every court appearance and census schedule. Attempts to exit through conversion or migration carry documented penalties in property and succession.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_communities, payer,
    powerless, generational, trapped, continental).

% Women whose marriage, widowhood, inheritance, and guardianship status the codified family law fixes. The codified rules narrowed several customary allowances that some communities had maintained, and made marital status a matter of state record rather than community practice. Any change of status runs through the same courts that apply the rules.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_under_codified_family_law, payer,
    powerless, biographical, trapped, continental).

% Scholars of the Sanskrit legal corpus. Early in the arrangement they serve as paid court advisers and their opinions carry weight in judgment; as the fixed code and its English commentaries mature, courts cite the code and precedent instead, and the advisory office withers. Some adapt by teaching or certifying the new system; others lose the livelihood and public standing that interpretive authority carried.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_pandit_interpreters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, traditional_pandit_interpreters, beneficiary).

% Lawyers, reform associations, and political movements arguing from the late nineteenth century onward that law for Indians should be made by Indians. They contest particular codified rules — age of consent, widow remarriage, caste entries — and the arrangement's authorship as such. They hold no seat in the code-making process until the final years of the interval.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indian_nationalist_reformers, excluded,
    organized, generational, constrained, continental).

% Scholars working after the arrangement's end who compare manuscripts, court records, and census schedules to reconstruct what the corpus contained before codification and what the codification changed. They hold no position inside the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, postcolonial_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Made a vast, legally plural territory administrable by a thin foreign administration: fixed a canonical text-base, made marriage, inheritance, and caste disputes adjudicable by rule rather than by case-by-case recourse to local experts, and rendered households and communities legible for census, taxation, and court process.
% TRANSFER_FUNCTION: Moves land revenue, court fees, and administrative compliance from colonized subjects to the colonial treasury; moves interpretive authority over family and caste matters from pandits and customary assemblies to colonial courts; converts fluid community identities into fixed, state-recorded categories.
% ABSENT_VOICES: The governed themselves: colonized subjects were the sources and objects of the codification, never participants — pandits were consulted as informants, not representatives; lower-caste communities and women, whose family-law position the codified rules fixed most tightly, had no seat at all; nationalist legal thinkers who wanted self-directed law reform entered the conversation only in the twentieth century. The unanimity of the 'recovered law' rested on rooms no governed person stood in.
% DISAPPEARANCE_RATIONALE: Courts, land titles, succession judgments, census schedules, and caste entries all referenced the codified corpus. Overnight disappearance would have left adjudication without operative rules, unsettled decades of property decisions, and stripped the administrative grid the census and revenue system ran on. The legal world built on it would have had to rebuild itself — as in fact it did, gradually and deliberately, through the Hindu Code Bills after 1947.
% FOUNDING_PROBLEM: How a small foreign administration could govern a vast population with diverse, expert-mediated, Sanskrit-encoded legal traditions predictably and cheaply, while securing land revenue and administrative legibility.
% FOUNDING_PROBLEM_CORROBORATION: The benefiting party (colonial administration) no longer exists to attest anything. Outside attestation: the Constituent Assembly debates and the Hindu Code Bill proceedings of the 1950s treat the colonial codification as an instrument of alien rule requiring replacement; late-colonial nationalist legal reform materials and postcolonial historiography (manuscript philology and archive work on the census and courts) corroborate, from outside the benefiting parties, that the founding problem was colonial governance and that it ended in 1947.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.48 (end-state): by the reading's own lights the arrangement is legitimate administration, yet the reading's own record concedes revenue primacy — land tax and court fees flowed outward on a scale the administration itself defended as the purpose of governance. Suppression 0.60 is structural, not internalized: compulsory jurisdiction, progressive derecognition of customary and local forums, and succession penalties on conversion; the internalized-identity question is routed to an omega rather than folded into the scalar. Theater_ratio 0.42: a growing share of activity defends the 'ancient authority' framing as Anglo-Hindu case law drifted from any manuscript basis, and pandit consultation became increasingly ceremonial — real adjudication continued throughout, so theater stays below half. Accessibility_collapse 0.45: alternatives (customary forums, regional schools, sectarian reidentification) were partly closed but persisted in the shadows; nothing like natural-law closure. Resistance 0.55: the 1857 uprising, the consent-age and widow-remarriage controversies, litigant non-cooperation, and the nationalist legal revival all pressed against the arrangement. The suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: machinery built up sharply after 1857 (police, courts, registration), plateaued, then decayed as legitimacy drained after 1919. All three series run on ONE shared eight-point grid (1772/1800/1830/1858/1891/1929/1947/1956) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the administrator seat the arrangement is coordination it built and maintains — a legibility and adjudication infrastructure with a transitional mission, experienced as scaffold-like. From the trapped payer seats the same structure operates as enforced transfer with no exit — snare-flavored experience. The upper-caste elite seat splits internally: codified family law secures its property while codified category freezes its standing, so its computed type depends on which flow dominates. The pandit seat experiences a career-long slide from co-opted participant to displaced bystander. The engine derives these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the administration (writes and enforces, collects), the revenue establishment (predictable assessment), the legal profession (livelihood from the corpus), and upper-caste elites (net property gain despite political subordination — flagged as dual-positioned rather than overridden, since the coarse power-atom keying of overrides cannot separate them from other organized actors). Targets sit near the full-target end: colonized subjects generally, with lower castes and women bearing the sharpest category-fixing costs and the least exit. Pandits derive mixed directionality from their dual declaration. Nationalist reformers hold the excluded seat — they feed the consensus-provenance check (the 'recovered law' unanimity was manufactured in rooms the governed never entered), not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing India for a foreign administration — died in 1947, and the arrangement outlived it in residual form until the Hindu Code Bills deliberately replaced it. This is mandatrophy resolved by legislative dismantling, not by atrophy: the late-interval phase (status=dead x world_rearranges) is exactly the capture/zombie configuration the mismatch consumer flags, and the flag is appropriate here. Classification discipline prevents two opposite errors: calling the arrangement pure extraction erases the genuine coordination achievement (uniform adjudication and legibility across a legally plural continent, which the successor republic inherited and reused); calling it pure coordination erases the asymmetric transfer and the enforced freezing of social categories. The scaffold claim locates the truth: real coordination, real transfer, transitional justification, terminated transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the colonial_orientalist_reading of kernel vedic_corpus_social_prescription; the orthodox_varna_reading (divinely mandated varna order) and reformist_spiritual_reading (non-prescriptive spiritual cosmology) are separate constraints — how would adopting either sibling''s premise restructure this arrangement''s beneficiary and victim sets?',
    'Generate the two sibling stories and compare computed classifications. The disagreement is located in the corpus''s normative status (positive law vs. divine mandate vs. metaphor), which determines who counts as a victim of prescription at all.',
    'Under the orthodox reading, enforcement reads as piety rather than imposition and the victim set shifts to those defying the mandated order; under the reformist reading the entire prescriptive apparatus is misreading and the victim set collapses toward zero. The moderate epsilon authored here holds only for the colonial reading and must not be averaged across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    unified_code_historicity,
    'Did the Vedic/Dharmashastra corpus constitute a unified legal system prior to colonial codification, or was ''unified timeless Hindu law'' substantially an Orientalist construction projected onto a stratified, regionally variant, practice-responsive literature?',
    'Manuscript-level philological comparison across regions and periods (the Rocher-Lingat-Davis line), measuring variance in doctrine, procedure, and authority structure across the corpus independent of colonial editions.',
    'If unity is constructed, the arrangement''s epsilon is attributable to the reading itself and the scaffold''s coordination claim weakens toward cover; if the corpus was substantially unified, part of the measured rigidity predates the arrangement and the reading''s recovery claim gains warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_code_historicity, empirical, 'Whether the codified unity existed in the corpus or was produced by the codification.').

omega_variable(
    codification_freezing_causality,
    'Did codification freeze previously fluid jati and customary mobility into fixed legal categories, or did it accelerate and record a hardening trend already underway?',
    'Pre-colonial mobility records (sectarian affiliation shifts, migration, service-role changes) compared against colonial-era court and census series for the same populations.',
    'Determines how much of the victim harm is attributable to this constraint versus a background trend — and therefore whether dismantling the codification (as the Hindu Code Bills did) could restore mobility or merely re-label a hardened order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_freezing_causality, empirical, 'Causal weight of codification versus background social hardening in producing fixed categories.').

omega_variable(
    sunset_sincerity,
    'Was the transitional justification (stewardship preparing India for self-governance, carrying an implicit sunset) a genuine terminal commitment, or cover for an arrangement intended to persist indefinitely?',
    'Internal administrative correspondence and policy papers: did any planning horizon treat the codified regime as provisional, or did successive administrations treat it as permanent infrastructure with the transition rhetoric doing purely legitimating work?',
    'If the sunset was sincere in structure, the scaffold classification holds; if it was cover, the arrangement is better read as a tangled rope whose transitional story was extraction''s alibi, and the late-interval zombie phase was the steady state, not a residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_sincerity, conceptual, 'Scaffold-versus-tangled-rope boundary: sincerity of the transitional justification.').

omega_variable(
    internalized_vs_structured_suppression,
    'Is the persistence of codified caste categories after 1947 driven by remaining structural law or by internalized identity — categories that survived the dismantling of the arrangement that fixed them?',
    'Post-1956 legal-change trajectory versus attitude and category-persistence data: where formal law changed quickly but category behavior persisted, weight internalization; where persistence tracked remaining legal hooks, weight structure.',
    'If internalized, the arrangement''s effective suppression exceeds its structural measure — the targets carried the categories beyond the arrangement''s death, raising the true cost of the codification above what the scalar records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structured_suppression, empirical, 'Structural versus internalized component of the suppression the codified categories produced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1772, 1956).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1772, 0.18).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.24).
narrative_ontology:measurement(vedi_tr_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1830, 0.31).
narrative_ontology:measurement(vedi_tr_t1858, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1858, 0.37).
narrative_ontology:measurement(vedi_tr_t1891, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1891, 0.43).
narrative_ontology:measurement(vedi_tr_t1929, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1929, 0.46).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.44).
narrative_ontology:measurement(vedi_tr_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1956, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1772, 0.34).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(vedi_be_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1830, 0.49).
narrative_ontology:measurement(vedi_be_t1858, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1858, 0.55).
narrative_ontology:measurement(vedi_be_t1891, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1891, 0.58).
narrative_ontology:measurement(vedi_be_t1929, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1929, 0.55).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.51).
narrative_ontology:measurement(vedi_be_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1956, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1772, 0.4).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.46).
narrative_ontology:measurement(vedi_su_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1830, 0.53).
narrative_ontology:measurement(vedi_su_t1858, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1858, 0.64).
narrative_ontology:measurement(vedi_su_t1891, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1891, 0.66).
narrative_ontology:measurement(vedi_su_t1929, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1929, 0.63).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.61).
narrative_ontology:measurement(vedi_su_t1956, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1956, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Hindu law' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel (vedic_corpus_social_prescription). This file is the colonial_orientalist_reading (scaffold; moderate epsilon; beneficiary = colonial administration; victims = colonized legal subjects under codified caste law). The orthodox_varna_reading is upstream: the colonial reading cited orthodox content as evidence of the corpus's authentic prescriptive force, and its codification hardened the orthodox hierarchy into enforceable law — an influence edge, not foreclosure, since traditionalist commitment survives independently. The reformist_spiritual_reading is downstream-reactive: its 'no prescriptive content' claim is defined partly against the state-fixed prescriptive version the colonial reading produced. Each member carries its own epsilon, beneficiaries, and victims; no member averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
