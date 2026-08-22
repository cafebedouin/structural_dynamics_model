% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II as Organic Magisterial Continuity
 *   domain: ecclesiastical/theological/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) produced texts that introduced significant changes
 *   in Church practice and pastoral orientation: ecumenism, religious
 *   freedom, vernacular liturgy, episcopal collegiality. The conciliar
 *   documents contain ambiguities and genuine tensions with prior magisterial
 *   positions — most notably the reconciliation of Dignitatis Humanae
 *   (religious freedom) with the Syllabus of Errors, and the deployment of
 *   liturgical reform while Sacrosanctum Concilium §36 mandated Latin
 *   preservation. The continuity reading interprets Vatican II as organic
 *   development within an unbroken magisterial tradition, arguing that the
 *   conciliar texts themselves constrain implementation to preserve
 *   pre-conciliar doctrine and that 'spirit of the Council' claims exceed the
 *   texts' authorization. This reading institutionalizes hermeneutical
 *   control: it frames the Curia as the arbiter of what counts as continuity.
 *   The constraint extracts interpretive authority from traditionalist
 *   bishops and reform theologians by rendering their readings as either
 *   unauthorized reinterpretation or failure to read the texts charitably.
 *   The claim/metric divergence is deliberate: the constraint is CLAIMED as
 *   tangled_rope (genuine coordination function + asymmetric extraction) and
 *   the authored metrics are consistent with that claim — high extractiveness
 *   (0.68), substantial suppression (0.71), and moderate theater (0.42)
 *   tracking the rise in hermeneutical policing over the post-conciliar
 *   decades.
 *
 * KEY AGENTS:
 *   - Roman Curia leadership: agenda-setter, controls hermeneutical apparatus, maintains continuity frame as binding.
 *   - Progressivist theology coalition: organized payers, constrained by continuity frame's authority structure from reading 'beyond the texts.'
 *   - Traditionalist episcopal factions: payers at moderate power, forced to fight hermeneutical battles on the Curia's turf.
 *   - Manual Latin liturgy practitioners: powerless payers and excluded, trapped between continuity reading and lived liturgical rupture.
 *   - Academic ecclesiology: analytical observers, produce evidentiary base but carry no magisterial standing.
 *   - Conciliar texts themselves: the kernel that multiple readings interpret; genuine tensions enable the continuity frame to work but also enable rival readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II as Organic Magisterial Continuity").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiastical/theological/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'efb191ac-9d50-4f2d-ae5b-b383d5fded25').
narrative_ontology:cs_kernel_codification('efb191ac-9d50-4f2d-ae5b-b383d5fded25', fixed_text).
narrative_ontology:cs_authority_grounding('efb191ac-9d50-4f2d-ae5b-b383d5fded25', extraction).
narrative_ontology:cs_interpretation_layer_present('efb191ac-9d50-4f2d-ae5b-b383d5fded25').
narrative_ontology:cs_reading_relation('efb191ac-9d50-4f2d-ae5b-b383d5fded25', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('efb191ac-9d50-4f2d-ae5b-b383d5fded25', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('efb191ac-9d50-4f2d-ae5b-b383d5fded25', foundational, magisterial_coherence_unbroken).
narrative_ontology:cs_axiom_status(magisterial_coherence_unbroken, holdable).
narrative_ontology:cs_axiom_grounding('efb191ac-9d50-4f2d-ae5b-b383d5fded25', magisterial_coherence_unbroken, deontological).
narrative_ontology:cs_axiom('efb191ac-9d50-4f2d-ae5b-b383d5fded25', foundational, conciliar_texts_constrain_implementation).
narrative_ontology:cs_axiom_status(conciliar_texts_constrain_implementation, holdable).
narrative_ontology:cs_axiom_grounding('efb191ac-9d50-4f2d-ae5b-b383d5fded25', conciliar_texts_constrain_implementation, conventional).
narrative_ontology:cs_reference_frame('efb191ac-9d50-4f2d-ae5b-b383d5fded25', papal_infallibility_and_organic_development_framework).
narrative_ontology:cs_drift_state('efb191ac-9d50-4f2d-ae5b-b383d5fded25', contemporary_post_conciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efb191ac-9d50-4f2d-ae5b-b383d5fded25', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, magisterial_authority_structure).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, conciliar_interpretation_apparatus).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_episcopal_factions).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, manual_latin_liturgy_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressivist_theology_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the conciliar texts via the continuity hermeneutical frame. Controls which readings are authorized and which are delegitimized. Maintains the claim that Vatican II represents organic development and that the 'spirit of the Council' exceeds textual authorization. Cannot exit without dissolving its institutional authority; is trapped by the need to maintain magisterial coherence.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, roman_curia_leadership, agenda_setter,
    institutional, generational, trapped, universal).

% Reads Vatican II as authorizing substantial reinterpretation of doctrine. Constrained by the Curia's continuity frame from pursuing their most expansive readings of the Council's 'spirit.' Can exit via secular academia or schism, but exit means loss of institutional voice. Pay the cost of having their readings delegitimized as unauthorized interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressivist_theology_coalition, payer,
    organized, biographical, constrained, global).

% Read Vatican II as rupture from pre-conciliar doctrine. Constrained to either accept the continuity frame (and obey its rulings) or risk institutional discipline. Fight on the Curia's hermeneutical turf where continuity is the default binding frame. Can exit via schism (SSPX model) or quiet non-compliance, but constrained within institutional Catholicism.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_episcopal_factions, payer,
    moderate, biographical, constrained, regional).

% Argue that SC §36's Latin preservation mandate was binding. Trapped between the continuity reading's claim to honor SC §36 and the historical reality of liturgical displacement. Have no interpretive standing to adjudicate the contradiction. Exit paths are schism or secular withdrawal; institutional Catholicism traps them.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, manual_latin_liturgy_practitioners, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, manual_latin_liturgy_practitioners, excluded).

% The conciliar documents themselves, treated as an authority kernel. Contains genuine ambiguities (religious freedom vs. Syllabus; Latin preservation vs. vernacularization) that enable the continuity reading to frame tensions as resolvable through development of doctrine or thesis/hypothesis distinction. The same ambiguities enable rival readings to claim the texts encode rupture or overdetermination.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, second_vatican_council_texts, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__continuity_reading, second_vatican_council_texts).

% Produces historical and hermeneutical scholarship on conciliar intent and the Vatican II texts. Analyzes whether the continuity frame is textually defensible or requires interpretive work beyond what the texts support. Carries no magisterial standing but generates evidentiary base that either supports or undermines the continuity reading's claims to textual fidelity.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, academic_ecclesiology, observer,
    analytical, generational, analytical, global).

% The rupture and overdetermination readings remain live in academic and reform circles but are systematically delegitimized by the continuity frame's institutional authority. Proponents must operate within the Curia's hermeneutical bounds or risk institutional sanction. Their exclusion from authoritative interpretation is maintained by the Curia's control of the magisterial apparatus.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rival_hermeneutical_readings, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, roman_curia_leadership).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the institutional coherence of the Catholic magisterium across a historical boundary (Vatican II) that could have fractured it. Enables the Church to claim that doctrinal shifts (ecumenism, religious freedom, liturgical reform) are development rather than reversal, preserving the premise that papal authority is continuous and binding across centuries.
% TRANSFER_FUNCTION: Transfers hermeneutical authority to the Curia and to institutional interpreters who control what counts as 'continuity' and 'organic development.' Moves interpretive power away from traditionalist-reading bishops and academic dissenters toward a centralized apparatus that frames implementation. Charges the cost to those whose readings of the texts exceed what the continuity frame permits.
% ABSENT_VOICES: Conciliar bishops who authored ambiguous or internally contradictory texts (now deceased) cannot clarify intention. Academic historians who argue the texts encode rupture or overdetermination are systematically excluded from magisterial hermeneutics. Lay Catholics whose lived experience was the 1960s-70s disruption have no seat in the theological apparatus that retroactively frames what happened.
% DISAPPEARANCE_RATIONALE: If the continuity reading and its enforcement machinery disappeared, the Church would either splinter (rival readings become equally authoritative) or reorganize around a different binding frame. The institutional unity of the post-conciliar Church depends on the continuity frame holding. Its loss would require complete re-adjudication of which conciliar changes were authorized and which were not.
% FOUNDING_PROBLEM: Vatican II produced texts that contained genuine tensions with prior magisterial positions (e.g., religious freedom vs. Syllabus of Errors; liturgical vernacularization vs. SC §36 Latin preservation). The Church needed a hermeneutical framework that could hold these positions as compatible without admitting rupture or error in prior teaching — both institutional coherence and papal infallibility doctrine required it.
% FOUNDING_PROBLEM_CORROBORATION: Magisterial statements from Paul VI onward repeatedly emphasize hermeneutical continuity and cite organic development doctrine. Academic historians and ecclesiologists (e.g., John O'Malley, Klaus Schatz) document that the texts genuinely DO contain tensions and that the continuity reading requires interpretive work — their analysis supports that a binding frame was needed. Traditionalist and reform bishops' testimony across decades confirms the tensions existed and that the continuity frame was imposed as the authoritative reading, not discovered in the texts themselves.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (immediate post-conciliar period, when the continuity frame was still being asserted) to 0.68 (steady state, ~30 years out, when the Curia's hermeneutical control solidified). The plateau thereafter (plateau-stage after year 40) reflects that the frame has become institutionalized — further extraction requires only maintenance, not new enforcement. Theater ratio follows a similar arc: minimal during the Council itself (the coordination function is genuine), rising as implementation diverges from SC §36 and lived experience contradicts the 'no rupture' claim (theater peaks at ~0.41 by year 30), then stabilizing as the performative work becomes normalized (the Latin liturgy 'preservation' becomes rhetorical, the continuity 'development' becomes catechesis). Suppression rises monotonically because the Curia must actively police the hermeneutical boundaries — progressivists are pushed back from the 'spirit of Vatican II' framing, traditionalists are disciplined for reading rupture, academics are marginalized from magisterial interpretation. The measurement points are authored on a single shared grid (all three metrics at all eight time points) so temporal analysis is not distorted by misaligned grids.
 *
 * PERSPECTIVAL GAP:
 *   The Curia and progressivist coalition experience this constraint from opposite directions. From the Curia's seat: the constraint is genuine coordination (holding the Church together through hermeneutical coherence) with a minimal extraction premium (the interpretive authority is necessary institutional overhead). From the progressivist theologian's seat: the same structure is majorly extractive (their readings are delegitimized, their interpretive authority is neutered). From the traditionalist bishop's seat: the extraction is extreme — the continuity frame forces them either to deny their own reading of what happened or to accept institutional discipline. From the powerless Latin liturgy practitioner: the extraction is absolute — SC §36 was binding, the continuity reading claims to honor it, but implementation did not; trapped between two incompatible claims, with no power to adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   The Curia and conciliar-interpretation apparatus are beneficiaries: they expand and consolidate interpretive authority through the continuity frame. The directionality flows strongly toward them (d near 0.0 for full beneficiary). Progressivist theologians and traditionalist bishops are payers: they lose interpretive autonomy; their readings are now subject to Curia arbitration. The directionality for them is high (d toward 0.8–0.9 for targets). Latin liturgy practitioners are most extreme: they paid the cost of liturgical displacement while the continuity reading claims there was no rupture — maximum extraction and suppression. The progressivists retain some exit through academic careers and reform movements outside strict magisterial control; traditionalists have schism or discipline as exits; Latin practitioners have only schism or spiritual withdrawal. The exit-option differential amplifies directionality divergence across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Vatican II's founding problem (tension between pre-conciliar and post-conciliar positions) is LIVE — the tension still exists and is never fully resolved by the continuity frame. The continuity reading does NOT solve the tension; it names it solved via development-of-doctrine or thesis/hypothesis distinction. When you ask traditionalists whether religious freedom really flows from organic development of Syllabus teaching, they say no — the tension persists. The mandatrophy signature appears here: a constraint that persists by insisting the founding problem is solved when the founding problem is still contested. The Curia has a mandate to preserve magisterial coherence; it has authorized the coherence claim and is now obliged to defend the coherence frame against challenges. The challenge never stops coming (traditionalist readings, papal ambiguity, scholarly work on conciliar intent) because the underlying tensions are real. The constraint persists not because it solved the founding problem but because the Curia's institutional authority is bound up with the claim that it did. Mandatrophy declared: yes, the founding problem (doctrinal tension) outlived the constraint's ability to resolve it; the constraint now persists by performative insistence rather than by structural resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organic_development_criterion_ambiguity,
    'What distinguishes ''organic development of doctrine'' from ''rupture with prior teaching''? Does Vatican II''s text-driven boundary between continuity and change rest on a principled criterion or on interpretive fiat?',
    'Systematic analysis of John Henry Newman''s development-of-doctrine framework (Church Fathers, 19th-century Catholic theology) versus 20th-century scholarship on doctrinal change (Erik Parens, Romanus Cessario). Compare the two criteria: Does Vatican II fit Newman''s criteria for genuine development? If yes, the continuity reading is defensible as textual claim. If no, the continuity is performative rather than structural.',
    'If development criterion is rigorous and Vatican II fits it, the continuity reading computes as tangled_rope (genuine coordination + extraction overhead). If the criterion is loose or Vatican II doesn''t fit it, the reading computes as snare (extraction with cover story). The mandatrophy claim stands either way — the founding problem isn''t resolved — but the institutional legitimacy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_development_criterion_ambiguity, conceptual, 'Whether organic development is a principled boundary or interpretive cover.').

omega_variable(
    sacrosanctum_concilium_36_binding_force,
    'Does Sacrosanctum Concilium §36''s mandate to preserve Latin in the liturgy bind conciliar implementation, or does it express a preference compatible with substantial vernacularization?',
    'Textual analysis of SC §36 alongside conciliar voting records, Paul VI''s Ecclesiae Sanctae clarifications, and the historical record of implementation (1965–1970). If SC §36 is binding and implementation violated it, the continuity frame has broken its own constraint. If SC §36 is merely advisory, the continuity reading is correct but has paid a high theater cost (claiming to preserve what it authorized to discard).',
    'High binding force + violated implementation = mandatrophy evidence strengthens, theater ratio justifies as ~0.45+, Latin practitioners'' suppression is vindicated as real (not insubordination but constraint-breaking). Low binding force = continuity frame holds but theater cost is real (the ''preservation'' is rhetorical). Either way, this omega resolves whether the continuity reading can claim textual fidelity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacrosanctum_concilium_36_binding_force, empirical, 'Whether SC §36 Latin preservation mandate is binding or advisory.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of traditionalist and progressivist readings structural (institutional discipline, magisterial sanction, exclusion from interpretive apparatus) or internalized (traditionalists and progressivists have accepted the continuity frame as binding logic)?',
    'Testimony from theologians and bishops across the post-conciliar decades: Do dissenters describe suppression as external coercion (discipline, career damage, magisterial rebuke) or as accepted epistemic authority? Do they continue to hold rival readings and merely conceal them, or have they internalized the continuity frame as legitimate? Post-exit suppression trajectory: When traditionalist groups separate (SSPX), does their resistance to the continuity reading persist, indicating structural suppression was primary? When progressivists enter secular academia, do their readings expand, indicating internalized suppression was operative?',
    'Structural suppression only: the constraint''s suppressiveness is external; removal of enforcement would liberate rival readings. Internalized + structural: the constraint has colonized the interpretive imagination; removal of enforcement would not fully restore rival readings'' legitimacy. High internalization suggests the continuity reading has become a default framing, not a contested imposition — theater ratio should be lower, extraction should feel natural rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in doctrinal interpretation.').

omega_variable(
    religious_freedom_reconciliation_principle,
    'Does the thesis/hypothesis distinction or development-of-doctrine framework genuinely reconcile Dignitatis Humanae with the Syllabus of Errors, or does it perform reconciliation while leaving the substantive tension unresolved?',
    'Systematic comparison of Syllabus doctrine (state must not tolerate heresy in the public sphere) with DH doctrine (conscience has inviolable right to religious freedom including public practice). Test whether thesis/hypothesis distinction (the Syllabus states the thesis—ideal doctrine for a Catholic state; DH states the hypothesis—practical accommodation when pluralism exists) is logically coherent or is equivocation. Examine whether development of doctrine (understanding of freedom deepens from the 1860s to 1965) is authentic doctrinal maturation or rebranding.',
    'Coherent reconciliation: the continuity reading is defensible as principled reinterpretation. Equivocation: the reading is performative, theater rises, mandatrophy strengthens. This is the crux omega for the entire continuity frame — if religious freedom cannot be reconciled with the Syllabus, the founding problem persists and mandatrophy is declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_freedom_reconciliation_principle, conceptual, 'Whether thesis/hypothesis or development framework genuinely reconciles contradictory pre- and post-conciliar doctrine.').

omega_variable(
    kernel_reading_overdetermination,
    'Does this reading (continuity) genuinely describe a unified constraint, or is it one facet of an overdetermined composite kernel that encodes multiple incompatible ecclesiastical visions and frames them as continuous?',
    'Compare this constraint story''s structural data (beneficiaries, victims, enforcement machinery, metrics) with the composite_overdetermination_reading story (when authored). If the overdetermination reading''s metrics are similar but its axioms are incompatible, the kernel itself may be overdetermined — the continuity reading is not wrong but is one partial framing of a multivalent structure. If the overdetermination reading''s metrics differ sharply, the readings are describing genuinely different constraints and are not rivaling interpretations of the same kernel.',
    'High overdetermination: the continuity reading is defensible but incomplete; mandatrophy applies to all readings simultaneously (the founding problem is genuinely unresolvable because the kernel encodes incompatible commitments). If true, this reading should be marked as a reading of an overdetermined kernel, not a reading of a univalent contested kernel. If low overdetermination: the readings are genuine rivals, mandatrophy applies selectively (whichever reading is institutionalized must continue insisting the problem is solved).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_overdetermination, conceptual, 'Whether the kernel is univalent-but-contested or overdetermined-composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(vati_be_t6, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(vati_be_t12, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(vati_su_t6, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(vati_su_t12, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, post_vatican_ii_liturgical_reform_implementation).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, ecumenism_institutional_mandate).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel vatican_ii_magisterial_authority. The continuity_reading interprets Vatican II as organic doctrinal development; the rupture_reading interprets it as fundamental ecclesiological break; the composite_overdetermination_reading interprets it as ambiguous encoding of incompatible positions. Each reading has different beneficiary structures, different extraction profiles, and different vulnerability to mandatrophy evidence. They are not perspectives on one constraint but separate constraints that share a common kernel and compete for institutional authority. The network affects_constraints link to sibling readings and to downstream constraints (liturgical implementation, ecumenical engagement) that inherit the hermeneutical framework established by this reading's institutional embedding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, organized, 0.82).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, moderate, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
