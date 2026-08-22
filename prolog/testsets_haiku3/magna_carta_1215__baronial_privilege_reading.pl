% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215: Baronial Privilege Reading — Feudal Contract Limited to Free Landowning Men
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta (1215) under the baronial privilege reading is a feudal
 *   contract between the English crown and the landowning nobility,
 *   extracting protections for the barons' property and legal status while
 *   leaving the unfree peasantry (roughly 60–70% of the population) and
 *   non-landowning freemen explicitly outside its protections. The charter's
 *   phrase 'free men' (liberi homines) refers to men of free legal status who
 *   hold substantial property—a tiny elite. The constraint operates as
 *   tangled_rope: it coordinates the barons' interest in limiting royal
 *   prerogative over their lands and rights, while simultaneously extracting
 *   that coordination from the masses of the unfree and the landless, whose
 *   subjection is preserved and legitimized by the same feudal structure the
 *   charter reinforces. The reading instantiates one kernel interpretation;
 *   sibling readings (universal_rights, living_document) claim different
 *   constituencies and different ε values for the same historical text.
 *
 * KEY AGENTS:
 *   - landowning_barons: primary beneficiaries — protected from arbitrary royal takings of their lands and rights; consolidate their position as the mediating layer between crown and kingdom
 *   - english_crown: agenda-setter and constrained party — forced to accept limits on its feudal prerogatives by baronial military power; maintains the feudal structure overall
 *   - unfree_peasants: primary victims — remain outside all protections, their subjection to feudal obligations untouched and legitimized by the same institutional order the charter reaffirms
 *   - non_landowning_freemen: secondary victims — possess legal free status but lack property to protect under the charter's logic; excluded from its scope
 *   - women: tertiary victims — no legal personhood under feudal law; excluded entirely from consideration as rights-bearers
 *   - jewish_merchants: excluded/targeted — explicitly mentioned in the charter for restrictions on debt collection, subject to special feudal levies, singled out for exclusion
 *   - later readers (12th-17th centuries): interpreters who gradually reframe 'free men' to include all subjects; this reading's chief competitors come from their revisionist tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.62).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.71).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215: Baronial Privilege Reading — Feudal Contract Limited to Free Landowning Men").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '89d9209d-e129-4bf7-ab47-2a6e23fa0297').
narrative_ontology:cs_kernel_codification('89d9209d-e129-4bf7-ab47-2a6e23fa0297', formalized).
narrative_ontology:cs_authority_grounding('89d9209d-e129-4bf7-ab47-2a6e23fa0297', lineage).
narrative_ontology:cs_interpretation_layer_present('89d9209d-e129-4bf7-ab47-2a6e23fa0297').
narrative_ontology:cs_reading_relation('89d9209d-e129-4bf7-ab47-2a6e23fa0297', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('89d9209d-e129-4bf7-ab47-2a6e23fa0297', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('89d9209d-e129-4bf7-ab47-2a6e23fa0297', foundational, feudal_contract_scope_narrow).
narrative_ontology:cs_axiom_status(feudal_contract_scope_narrow, holdable).
narrative_ontology:cs_axiom_grounding('89d9209d-e129-4bf7-ab47-2a6e23fa0297', feudal_contract_scope_narrow, empirically_contingent).
narrative_ontology:cs_axiom('89d9209d-e129-4bf7-ab47-2a6e23fa0297', foundational, free_men_landowning_elite_referent).
narrative_ontology:cs_axiom_status(free_men_landowning_elite_referent, holdable).
narrative_ontology:cs_axiom_grounding('89d9209d-e129-4bf7-ab47-2a6e23fa0297', free_men_landowning_elite_referent, empirically_contingent).
narrative_ontology:cs_reference_frame('89d9209d-e129-4bf7-ab47-2a6e23fa0297', baronial_feudal_contract_1215).
narrative_ontology:cs_drift_state('89d9209d-e129-4bf7-ab47-2a6e23fa0297', contemporary_constitutional_interpretation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('89d9209d-e129-4bf7-ab47-2a6e23fa0297', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, unfree_peasants).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landholding_freemen).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, jewish_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landowning_freemen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landowning English nobles with the military power to compel concessions from the crown. The charter protects their property rights from arbitrary royal seizure and secures their feudal privileges. They author the charter's language, set its scope to include themselves and exclude non-landowners, and enforce it through intermittent military pressure. Exit for them means seeking support from rival claimants to the throne or from foreign powers—a real option for a landowning elite with resources.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% The English monarchy is forced by baronial power to accept legal limits on its feudal prerogatives—specifically, to cease arbitrary takings of barons' lands and to observe due process in cases involving baronial property. The crown maintains overall sovereignty and the feudal structure but must negotiate with the barons. Its exit options are constrained: it can violate the charter and face rebellion (as John did in 1215), or it can rule within the agreed limits.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, english_crown, agenda_setter,
    institutional, generational, constrained, national).

% Serfs and bondspeople comprise the majority of the population (~60–70%). They are legally unfree, bound to the land, and subject to feudal labor obligations to their lords. The charter explicitly does not protect them—its scope is limited to 'free men,' a category from which they are excluded by legal status. Their subjection is reaffirmed and legitimized by the same feudal structure the charter protects. Exit is not available: they cannot migrate without their lord's permission, cannot appeal to law in their own right, and have no property to protect under any legal regime. Resistance is limited to flight or collective uprising (the Peasant Revolt is a later manifestation).
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_peasants, payer,
    powerless, biographical, trapped, local).

% Free men who own little or no land—burgesses, artisans, small merchants, yeomen farmers. They possess legal free status (unlike serfs) but lack the property holdings that make the charter's protections meaningful. They are excluded from the charter's scope because 'free men' in the baronial reading means landholding free men. Their exit options are constrained: they can migrate to another town or region (more mobile than serfs), but they cannot appeal to the law for protection in the way a landholder can, because the law is written for landholders.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowning_freemen, payer,
    moderate, biographical, constrained, regional).

% Women have no legal personhood under feudal law. The charter is written entirely in terms of 'free men' and males; women appear only as property (widows' marriages are crown prerogatives to be sold or disposed of). Exit is not available in any meaningful sense: a woman's legal status is determined entirely by her relation to men (father, husband, or brother). She cannot own property in her own right in most cases and cannot appeal to law as a rights-bearer.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women, payer,
    powerless, biographical, identity_locked, local).

% Jewish moneylenders and merchants are explicitly mentioned in the charter (Clause 10-11) for restrictions on debt collection and special crown levies. They are targeted for extraction: their debts to the crown are restricted (limiting their primary income), and they are subject to special taxes and restrictions unique to their religious status. Their exit options are severely constrained: they are subject to crown prerogatives over their property and movement, and they cannot appeal to the protection of the law the way Christian landholders can. This is structural exclusion by explicit mention.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, jewish_merchants, payer,
    moderate, biographical, constrained, national).

% Historians, legal scholars, and constitutional theorists across the medieval, early modern, and modern periods who read the charter in changing ways. From the 12th century onward, they gradually reframe 'free men' to include broader constituencies, and they build the interpretive tradition that transforms the charter from a feudal contract into a symbol of universal rights. They observe and participate in the reinterpretation process that shifts the charter's function from coordination-among-barons to a precedent for rights-claims by later groups.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, later_interpreters, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the barons' collective-action problem: they coordinate to limit the crown's feudal prerogatives (arbitrary takings of baronial lands, violation of inheritance customs, forced marriages of heirs) by codifying mutual obligations between crown and landowning class. This coordination strengthens the barons' ability to hold the crown to agreed norms.
% TRANSFER_FUNCTION: The arrangement transfers legal authority and protection FROM the crown TO the barons as a class. In exchange, the barons pledge (theoretically) loyalty and feudal service, but the charter guarantees their property rights and legal status in ways that strengthen their position. The unfree majority and women transfer nothing and receive nothing—they are excluded from the arrangement entirely.
% ABSENT_VOICES: The unfree peasantry (60–70% of the population), non-landowning freemen, women, and Jewish merchants are explicitly absent from the negotiation and the charter's protections. They would object to their exclusion, to the reaffirmation of feudal subjection, and to the charter's use as a legitimizing device for the feudal hierarchy that oppresses them. None of these groups had a voice in the charter's negotiation or ratification.
% DISAPPEARANCE_RATIONALE: If the baronial privilege reading's constraint (the feudal contract limiting royal prerogative) disappeared overnight, the barons would lose their codified legal protection against arbitrary royal takings, and the crown would regain its feudal prerogatives. The feudal hierarchy itself would remain, but the barons' legal security would collapse. The unfree majority would be largely unaffected—they would continue to be subordinated, but to what authority (a more powerful crown, or feudal fragments) would be contested.
% FOUNDING_PROBLEM: Arbitrary royal prerogative over baronial holdings and rights: the crown's power to seize barons' lands, exploit wardships, extract reliefs and fines without negotiation, and override baronial inheritance customs and due process in cases involving baronial property.
% FOUNDING_PROBLEM_CORROBORATION: Baronial chroniclers and royal justicars from the 13th century attest this problem: John's reign (1199–1216) is documented by contemporaries as a period of arbitrary takings and baronial grievance. Later legal historians and paleographers (Stubbs, Powicke, Holt) from outside the benefiting parties (modern scholars, not interested parties to the feudal regime) corroborate that royal prerogative over baronial property was indeed a live conflict in the period. The unfree majority never attests to this as THEIR founding problem—their grievances center on serfdom itself, which the charter does nothing to address.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the charter protects a narrow elite against the monarch while the enforcement structure preserves the unfree majority's subjection. The extraction is not from the barons (who benefit) but IS extracted from those outside the protected class—their exclusion is structurally upheld by the same enforcement machinery that protects barons' rights. Suppression is high (0.71) because maintaining the unfree majority's subordination requires continuous enforcement: labor obligations, legal disabilities, restricted movement. Theater rises over the interval (0.12 to 0.28) as the charter's actual scope (barons only) increasingly diverges from later interpretations claiming universal application—by the later medieval period the charter becomes more rhetorical cover for legitimacy than a functional description of who is actually protected. Accessibility_collapse is moderate (0.45) because alternatives to feudal subjection DO exist in principle for barons (they could resist the crown, form alternative kingdoms, or trade with other powers), but for the unfree, the alternatives to serfdom are genuinely inaccessible—exit is blocked by legal status, economic dependency, and limited geographic mobility. Resistance is high (0.68) because barons mounted armed force to compel the charter, and later centuries saw the unfree organizing peasant revolts explicitly in the name of freedom and law—they mounted what resistance they could against structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   The barons and the crown sit on opposite sides of enforcement: the barons experience the charter as coordination—a mechanism to constrain the king's prerogatives over their holdings. The crown experiences it as extraction—forced concessions wrung by baronial military power. The unfree majority experience it as irrelevant at best, and at worst as a mechanism that legitimizes their exclusion: the charter's very invocation of 'free men' implicitly defines some men as unfree, and that definition becomes constitutional law. From the analytical seat (later constitutional readers), the charter can be reframed as a rights precedent—but that reframing requires denying the original narrow scope, which is what the living_document and universal_rights readings do. This reading holds that scope strictly.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowning_barons sit at d near 0.1–0.2 (beneficiaries: gain legal protection, coordinate against the crown, consolidate feudal property rights). The crown sits at d near 0.5–0.6 (constrained: forced to yield prerogatives but retains feudal structure and overall sovereignty). Unfree_peasants and non_landowning_freemen sit at d near 0.85–0.95 (full targets: excluded from protection, suppressed by the enforcement machinery, have no exit except flight or revolt). Women sit at d near 1.0 (no legal personhood at all; the constraint literally does not name them as a category). Jewish_merchants sit at d near 0.80 (specifically restricted and taxed within the charter's language, targeted for extraction). The power asymmetry is extreme: the barons are institutional power that compels the constraint; the unfree are powerless and trapped. Exit_options range from arbitrage (barons can negotiate, seek alliances) to trapped (unfree cannot exit feudal status). This directionality structure is stable and does not require overrides—the derivation from beneficiary/victim declarations + power + exit naturally produces the observed seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The baronial privilege reading avoids false mandatrophy claims because it does not confuse the original charter's scope with later reframing. The founding_problem (royal prerogative over baronial holdings) is genuinely alive in 1215; it is NOT dead, only later transformed by reinterpretation. A mandatrophy analysis would apply only if the reading claimed the charter persists to solve a founding problem that is demonstrably resolved—but under this reading, the barons maintained the feudal structure, so their original problem (unchecked royal takings) remained live until the barons' own power declined centuries later. The theater_ratio's increase over time (0.12 to 0.28) reflects not mandatrophy but reinterpretation drift: the charter's actual function narrows (barons secure their position, then feudalism itself becomes anachronistic) while its rhetorical scope expands (later readers apply it to new constituencies). This is not the constraint losing function and persisting by inertia—it is the constraint's social carriers (barons) losing relevance while their charter becomes a symbol later groups claim for their own purposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_versus_doctrine_gap,
    'Is Magna Carta a settled doctrine of universal rights, or a feudal contract whose scope was originally limited to barons? Does the accumulated interpretive tradition constitute legitimate constitutional development, or misreading of a narrower original text?',
    'Structural analysis: compare the 1215 charter''s explicit language and beneficiary class against later readings. Hermeneutic dispute resolution turns on whether interpretive tradition can override original text or whether it has done so illegitimately.',
    'If the baronial reading is the correct one and tradition has misread it, the constraint''s classification shifts from a universal-rights mountain to a feudal extraction arrangement whose beneficiaries have rewritten its narrative. If the living-document reading is correct, the accumulated tradition IS the constraint, not a distortion of it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_doctrine_gap, conceptual, 'Whether Magna Carta''s legitimacy grounds in original narrow scope or accumulated tradition.').

omega_variable(
    free_men_definition_contest,
    'Does ''free men'' in 1215 refer only to landowning barons and knights, or was the term already capacious enough to include non-landowning freemen? How did the status of the unfree majority interact with the baronial protection set?',
    'Paleographic and demographic analysis of 1215 England: census records, manorial documents, and legal records establishing the proportions of unfree, non-landowning free, and landowning men. Cross-reference the charter''s explicit language against contemporaneous usage in other documents.',
    'A narrow definition (barons and knights only) solidifies the extraction reading—the charter protects a tiny elite against the monarch while leaving the vast majority exposed. A broader definition (all freemen regardless of land) introduces an unrealized universalist potential within the original text, complicating the pure extraction frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_definition_contest, empirical, 'The actual referent class of ''free men'' in 1215 Plantagenet terminology.').

omega_variable(
    suppression_internalization_mechanism,
    'Why did the unfree majority accept exclusion from Magna Carta''s protections? Was suppression purely structural (legal prohibition, economic dependency), or did internalization play a role—did the unfree see their subjection as natural, deserved, or outside the frame of law entirely?',
    'Post-suppression trajectory: during periods when feudal structures weakened (e.g., the peasant revolts of the 14th century), how quickly did the unfree articulate grievances about their exclusion from Magna Carta? If suppression was internalized, the articulation would emerge slowly; if structural, grievances would surface as soon as exit became materially possible.',
    'If suppression is primarily structural, the constraint''s effective suppression is lower than the metric suggests—the unfree could exit/organize if barriers fell. If primarily internalized, the suppression persists even after structural barriers dissolve, and the constraint''s hold is deeper than the scalar measure captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural versus internalized suppression mechanism in the feudal subordination.').

omega_variable(
    kernel_scope_instability,
    'This reading instantiates one interpretation of the Magna Carta kernel. The kernel itself is the 1215 document as a stabilized text. But is the kernel''s scope fixed (the original 63 clauses as written in 1215), or does the kernel include the reissues of 1217 and 1225 with their modifications, and the later confirmation statutes? If the scope of the kernel itself shifts with reissue and confirmation, what counts as fidelity to the kernel versus departure from it?',
    'Hermeneutic analysis of how authorities have treated the ''kernel'' across 800+ years: which version (1215/1217/1225/later confirmations) is cited as authoritative, and when do authorities switch between versions? Does the existence of reissues constitute legitimate evolution of the kernel, or does it split the kernel into multiple versions, each with its own reading-space?',
    'If the kernel is fixed at 1215, this reading''s narrow scope is stable. If the kernel includes reissues, later readings (universal_rights, living_document) can claim fidelity to modified language. If reissues split the kernel into version-variants, there may be separate constraints per version, linked by the constraint family network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_scope_instability, conceptual, 'Whether the kernel (the stabilized text being read) is the 1215 original or includes subsequent reissues and confirmations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t1, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement_basis(magn_tr_t1, observed).
narrative_ontology:measurement(magn_tr_t2, magna_carta_1215__baronial_privilege_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement_basis(magn_tr_t2, observed).
narrative_ontology:measurement(magn_tr_t3, magna_carta_1215__baronial_privilege_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(magn_tr_t3, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t1, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1, 0.6).
narrative_ontology:measurement_basis(magn_be_t1, observed).
narrative_ontology:measurement(magn_be_t2, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 2, 0.61).
narrative_ontology:measurement_basis(magn_be_t2, observed).
narrative_ontology:measurement(magn_be_t3, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(magn_be_t3, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t1, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1, 0.69).
narrative_ontology:measurement_basis(magn_su_t1, observed).
narrative_ontology:measurement(magn_su_t2, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement_basis(magn_su_t2, observed).
narrative_ontology:measurement(magn_su_t3, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 3, 0.71).
narrative_ontology:measurement_basis(magn_su_t3, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Magna Carta 1215 kernel constraint family. Three structurally distinct constraints are emitted from the same 1215 text under three readings: the baronial_privilege_reading (this file) construes 'free men' narrowly as landowning nobles and finds extraction of the unfree majority; the universal_rights_reading construes 'free men' universally and emits a due-process mountain; the living_document_reading accepts the original narrow scope but argues accumulated tradition has transformed the constraint into a living constitutional substrate. The readings are siblings under the same kernel; they share no constraint_id but are linked via network.affects_constraints. The ε values differ substantially because the referent (what the charter is ABOUT) is the same standing arrangement, but the reading's endorsed interpretation changes what extraction that arrangement represents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
