% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Development (Continuity Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story authors the continuity reading of the Vatican II authority
 *   kernel: the position, articulated most explicitly by Pope Benedict XVI's
 *   2005 'hermeneutic of reform' address and defended by generations of
 *   conciliar theologians, that the sixteen documents of the Second Vatican
 *   Council represent organic development of the unchanging deposit of faith
 *   rather than a break with prior teaching. Under this reading, apparent
 *   tensions between conciliar texts and earlier magisterial statements (on
 *   religious liberty, ecumenism, collegiality) are resolved through
 *   traditional hermeneutical method, and post-conciliar reforms are
 *   legitimate precisely because they remain faithful to this properly-read
 *   text. This is ONE of three structurally distinct constraints sharing the
 *   kernel: the rupture reading (which holds the same texts contain
 *   irreconcilable doctrinal contradictions) and the
 *   composite-overdetermination reading (which holds the ambiguity is
 *   structurally irreducible) are separate stories with their own ε and their
 *   own stakeholders, linked via network.affects_constraints. This story does
 *   not describe or average over those readings; it authors only what the
 *   continuity reading itself claims and who it benefits.
 *
 * KEY AGENTS:
 *   - progressive_reform_clergy: primary beneficiary — reforms gain doctrinal legitimacy under this reading
 *   - conciliar_hermeneutics_theologians: agenda-setting beneficiary — supply and administer the interpretive apparatus
 *   - national_bishops_conferences: institutional beneficiary — expanded collegial authority legitimized
 *   - roman_curia_doctrinal_office: agenda_setter — polices the boundary of legitimate development
 *   - traditionalist_communities: excluded — hold the rupture reading, treated as resolved rather than live
 *   - ecumenical_dialogue_partners: observer — external assessor with no vote in the internal dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.18).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.32).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Doctrinal Development (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '9948cf91-14bf-4d50-986b-f67e8713ad36').
narrative_ontology:cs_kernel_codification('9948cf91-14bf-4d50-986b-f67e8713ad36', formalized).
narrative_ontology:cs_authority_grounding('9948cf91-14bf-4d50-986b-f67e8713ad36', lineage).
narrative_ontology:cs_interpretation_layer_present('9948cf91-14bf-4d50-986b-f67e8713ad36').
narrative_ontology:cs_reading_relation('9948cf91-14bf-4d50-986b-f67e8713ad36', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('9948cf91-14bf-4d50-986b-f67e8713ad36', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('9948cf91-14bf-4d50-986b-f67e8713ad36', foundational, doctrinal_development_preserves_deposit_of_faith).
narrative_ontology:cs_axiom_status(doctrinal_development_preserves_deposit_of_faith, holdable).
narrative_ontology:cs_axiom_grounding('9948cf91-14bf-4d50-986b-f67e8713ad36', doctrinal_development_preserves_deposit_of_faith, deontological).
narrative_ontology:cs_axiom('9948cf91-14bf-4d50-986b-f67e8713ad36', foundational, traditional_hermeneutics_resolve_apparent_textual_tension).
narrative_ontology:cs_axiom_status(traditional_hermeneutics_resolve_apparent_textual_tension, holdable).
narrative_ontology:cs_axiom_grounding('9948cf91-14bf-4d50-986b-f67e8713ad36', traditional_hermeneutics_resolve_apparent_textual_tension, conventional).
narrative_ontology:cs_reference_frame('9948cf91-14bf-4d50-986b-f67e8713ad36', conciliar_teaching_as_organic_development).
narrative_ontology:cs_drift_state('9948cf91-14bf-4d50-986b-f67e8713ad36', post_benedict_xvi_address_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('9948cf91-14bf-4d50-986b-f67e8713ad36', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reform_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_hermeneutics_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, national_bishops_conferences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy and religious educators who implement liturgical, pastoral, and ecumenical reforms (vernacular liturgy, collegiality, religious liberty teaching) and rely on the continuity reading to certify these changes as faithful development rather than departure. Their pastoral authority and institutional standing depend on the reforms being read as legitimate.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reform_clergy, beneficiary,
    organized, generational, constrained, global).

% Theologians (in the tradition of the 'hermeneutic of reform in continuity' articulated by Benedict XVI and taken up by pontifical commissions) who supply the interpretive apparatus reconciling the sixteen documents with prior magisterium. They administer the reading through seminary curricula, doctrinal commissions, and papal teaching offices.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, conciliar_hermeneutics_theologians, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, conciliar_hermeneutics_theologians, agenda_setter).

% Regional episcopal bodies that gained expanded collegial authority under conciliar teaching (Lumen Gentium, Christus Dominus) and depend on the continuity reading to legitimize their enlarged administrative and doctrinal role relative to the Roman curia.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, national_bishops_conferences, beneficiary,
    institutional, generational, constrained, continental).

% Communities and clergy (e.g. associated with the Society of St. Pius X and allied movements) who hold the rupture reading and are treated within this reading's framework as in error or schismatic rather than as parties to a live theological dispute. Their objection — that specific texts (religious liberty, ecumenism, collegiality) cannot be harmonized with prior condemnations — is not adjudicated inside the continuity reading; it is presumed answered.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_communities, excluded,
    moderate, generational, constrained, global).

% The papal magisterium and its doctrinal congregation adjudicate disputed conciliar interpretation, issuing clarifications (e.g. Dignitatis Humanae commentary, Dominus Iesus) that operate within the continuity framework to police the boundary between legitimate development and rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, roman_curia_doctrinal_office, agenda_setter,
    institutional, civilizational, constrained, global).

% Non-Catholic Christian bodies and observers who assess whether conciliar ecumenical and interreligious teaching represents genuine doctrinal movement; they have a stake in whether the continuity claim is credible but no vote inside the Catholic authority structure.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ecumenical_dialogue_partners, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interpretive framework allowing the sixteen conciliar documents, prior magisterial teaching, and post-conciliar pastoral practice to be read as one coherent, non-contradictory body of doctrine — solving the problem of how a church claiming doctrinal permanence can also visibly change its teaching and practice.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy toward those who administer the 'hermeneutic of continuity' (curial doctrinal offices, conciliar-trained theologians, reform-implementing bishops) and away from readings that would require either repudiating prior teaching (rupture reading) or declaring the council's authority indeterminate (composite reading). No material extraction is authored under this reading — the delta specifies reforms as cost-free development, so victims is empty.
% ABSENT_VOICES: Traditionalist communities holding the rupture reading are the primary excluded voice: they contest specific textual reconciliations (religious liberty, collegiality, ecumenism) as substantively incompatible with prior condemnations, and their objection is treated within this reading as resolved rather than live. Composite-reading theologians who hold the ambiguity itself is irreducible are also absent from this reading's framework.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared as the operative institutional framework, the practical liturgical and pastoral reforms would likely persist (they are decades-embedded in parish life), but their theological legitimacy would become an open question — bishops conferences and reform theologians would lose the doctrinal cover the reading provides, and the rupture or composite readings would gain institutional traction. Whether this counts as 'world rearranges' or 'world unchanged' is itself part of the kernel contest, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The Second Vatican Council (1962-65) was convened to address the Church's relationship to modernity — religious liberty, ecumenism, the laity's role, liturgical intelligibility — without either repudiating prior dogmatic teaching or freezing the Church against pastoral adaptation. The continuity reading was built to hold both commitments simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Benedict XVI's 2005 Christmas curial address explicitly articulated and endorsed the continuity hermeneutic from within the magisterium itself (an interested party). Outside the benefiting parties, historians of the council (e.g. the Bologna school associated with Giuseppe Alberigo) and traditionalist canonists dispute that the founding problem was ever coherently solved rather than papered over; no fully disinterested corroborating source exists, since every party to the dispute has a theological or institutional stake in which reading prevails.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because, by the continuity reading's own lights, the arrangement is not extractive at all — it is a hermeneutical framework enabling legitimate development, and the reading explicitly declares no victims (the expected structural delta specifies reforms as cost-free). The low-but-nonzero and slowly rising extractiveness reflects the accumulating institutional stakes theologians and bishops conferences have in the reading's survival (career and authority investment growing over sixty years), not extraction from an identified victim class. Suppression (0.32) is moderate rather than low: the reading is defended through doctrinal offices' authority to characterize alternative readings (rupture, composite) as errors requiring correction, which is a real if soft form of foreclosure on rival interpretation, but it operates through persuasion and magisterial teaching authority rather than coercive enforcement — hence requires_active_enforcement is false. Theater ratio is modest (0.22, slowly rising) reflecting some performative reconciliation-work in doctrinal commissions addressing traditionalist objections without fully engaging their substance.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive clergy, conciliar theologians, and bishops conferences are declared beneficiaries because the continuity reading directly legitimizes their pastoral and doctrinal authority — the reading is the ground on which their reforms stand as faithful rather than heterodox. No victims are declared because this reading structurally asserts that reforms are cost-free development; authoring a victim here would contradict the reading's own content (the rupture reading, a separate story, is where the victim set — those who hold prior teaching was violated — properly belongs). Traditionalist communities are excluded rather than victimized: they are not extracted from by this reading, they are simply not party to its framework — their grievance belongs to the sibling rupture_reading story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead: whether the reading's founding purpose (reconciling doctrinal permanence with pastoral change) remains live is exactly what separates this reading from its siblings. The mismatch check (status=contested x disappearance_verdict=contested) correctly declines to flag capture — this is not a zombie mandate, it is a genuinely disputed theological claim, which is the appropriate output for a kernel reading rather than a settled institutional artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_of_continuity_as_committer_choice,
    'Is the ''hermeneutic of reform in continuity'' a discovery of what the council texts actually mean, or a committer-level choice among several defensible framings imposed to preempt the rupture reading?',
    'Comparative textual analysis of specific loci (Dignitatis Humanae on religious liberty vs. Quanta Cura/Syllabus of Errors; Nostra Aetate vs. prior exclusivist ecclesiology) against the interpretive moves the continuity reading requires to harmonize them, cross-checked against how the rupture reading and composite reading characterize the same loci.',
    'If the harmonizing moves are textually strained rather than natural readings, the continuity reading functions more as an institutionally necessary cover story than a neutral interpretation — this would not change this story''s own ε (which is authored from the continuity reading''s own lights per the fixed-referent rule) but would strengthen the case for the rupture reading''s higher ε in its own story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_of_continuity_as_committer_choice, conceptual, 'Whether the continuity hermeneutic is discovered or imposed as a committer-level framing choice.').

omega_variable(
    no_victim_declaration_stability,
    'Does the continuity reading''s declaration of zero victims hold once traditionalist communities'' material losses (canonical status changes, liturgical rite restrictions, disciplinary actions against clergy holding the rupture reading) are considered?',
    'Track canonical and disciplinary actions taken against clergy and communities who publicly hold the rupture reading (e.g. restrictions on the pre-conciliar liturgical rite, canonical penalties) and assess whether these actions are downstream of the continuity reading''s institutional enforcement or of independent disciplinary logic.',
    'If disciplinary actions against rupture-reading holders are shown to be enforcement of the continuity reading specifically, this story''s victims array and requires_active_enforcement flag would need revision toward a tangled_rope classification; as authored, the reading''s own framework asserts these are separate governance matters, not extraction under this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(no_victim_declaration_stability, empirical, 'Whether traditionalist disciplinary consequences are properly attributable to this reading''s enforcement or are structurally independent.').

omega_variable(
    cs_framing_kernel_vs_document_set,
    'Should the kernel be framed as ''the sixteen conciliar documents as a fixed text'' (fixed_text codification) or as ''the magisterium''s ongoing authority to interpret them'' (formalized, lineage-grounded)? These framings could route to different cs_pattern classifications.',
    'Examine whether disputes in practice attach to textual wording (favoring fixed_text) or to the legitimacy of the interpreting authority itself (favoring formalized/lineage) — survey how each of the three readings argues its case.',
    'Under fixed_text framing, drift analysis would center on textual reception history; under formalized/lineage framing, it centers on the magisterium''s authority to adjudicate meaning. This story adopts formalized/lineage because the continuity reading''s own argument (per Benedict XVI''s address) is explicitly about interpretive method and authority, not textual fixity alone — but the alternative framing remains defensible and would shift emphasis in the cs_structure fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_document_set, conceptual, 'Alternative framing of the kernel as fixed-text versus authority-lineage, and which the continuity reading''s own argument supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_authority__continuity_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__continuity_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_authority__continuity_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.16).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__continuity_reading, base_extractiveness, 2015, 0.17).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vatican_ii_authority__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Part of the vatican_ii_authority kernel family (3 stories). continuity_reading (this story) authors low extraction and zero declared victims, consistent with its own claim that reforms are cost-free development. rupture_reading is expected to author substantially higher extraction with an identified victim class (those bound to accept teaching the reading holds is doctrinally erroneous or contradictory). composite_overdetermination_reading is expected to author a structural-ambiguity-driven classification (likely piton or tangled_rope) reflecting that no single coherent narrative — continuity or rupture — can be sustained across all sixteen documents. The continuity reading structurally forecloses the rupture reading (their core premises about doctrinal compatibility directly contradict) while coexisting with the composite reading (different theological methodologies, not a direct logical contradiction, since composite reading does not assert what continuity denies — it asserts irreducible plurality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
