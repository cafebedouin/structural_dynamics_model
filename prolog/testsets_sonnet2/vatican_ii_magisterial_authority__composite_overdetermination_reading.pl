% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Overdetermined Composite Text — Hermeneutical Control as the Real Authority
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II kernel: the claim that the conciliar texts are not a single
 *   coherent statement admitting one correct hermeneutic (continuity or
 *   rupture) but an engineered compromise that encodes both incompatible
 *   ecclesiologies simultaneously, in order to secure a supermajority vote
 *   across a divided episcopate. Under this reading, the real site of
 *   institutional power shifted from the text itself to whoever controls its
 *   authoritative interpretation post-ratification, and the documented 10-12%
 *   rejection votes on contested schemas are read as a residue of genuinely
 *   unresolved theological incompatibility rather than mere dissent to a
 *   settled matter. This is a tangled rope: the compromise drafting solved a
 *   real and urgent coordination problem (averting schism at the Council),
 *   but the same ambiguity that solved it now sustains an asymmetric
 *   extraction — central hermeneutical authority, regional episcopal
 *   conferences, and academic theology all draw ongoing institutional benefit
 *   from perpetuated ambiguity, while traditionalist and progressive
 *   minorities and ordinary parish faithful bear the cost of unresolved
 *   doctrinal instability.
 *
 * KEY AGENTS:
 *   - curial_hermeneutical_office: agenda_setter (institutional/arbitrage) — administers the authoritative reading of ambiguous texts
 *   - national_episcopal_conferences: beneficiary/agenda_setter (organized/constrained) — exploit ambiguity for regional pastoral latitude
 *   - post_conciliar_theological_faculties: beneficiary (organized/mobile) — sustain scholarly economy on unresolved interpretation
 *   - traditionalist_communities: payer (powerless/trapped) — bear cost of rupture reading being practically imposed
 *   - progressive_reform_movements: payer (moderate/constrained) — bear cost of continuity reading being reasserted after the fact
 *   - ordinary_parish_faithful: payer (powerless/constrained) — absorb downstream instability without visibility into the ambiguity
 *   - conciliar_periti_and_drafting_commissions: excluded (moderate/trapped) — original drafters whose testimony to intentional ambiguity is archival only
 *   - ecclesiastical_historians: observer (analytical/analytical) — document the engineered-compromise structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.44).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II as Overdetermined Composite Text — Hermeneutical Control as the Real Authority").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '10513884-2eb1-477c-ad7e-4cf13870c27a').
narrative_ontology:cs_kernel_codification('10513884-2eb1-477c-ad7e-4cf13870c27a', fixed_text).
narrative_ontology:cs_authority_grounding('10513884-2eb1-477c-ad7e-4cf13870c27a', extraction).
narrative_ontology:cs_interpretation_layer_present('10513884-2eb1-477c-ad7e-4cf13870c27a').
narrative_ontology:cs_reading_relation('10513884-2eb1-477c-ad7e-4cf13870c27a', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('10513884-2eb1-477c-ad7e-4cf13870c27a', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('10513884-2eb1-477c-ad7e-4cf13870c27a', foundational, conciliar_texts_are_overdetermined_by_design).
narrative_ontology:cs_axiom_status(conciliar_texts_are_overdetermined_by_design, holdable).
narrative_ontology:cs_axiom_grounding('10513884-2eb1-477c-ad7e-4cf13870c27a', conciliar_texts_are_overdetermined_by_design, empirically_contingent).
narrative_ontology:cs_axiom('10513884-2eb1-477c-ad7e-4cf13870c27a', foundational, hermeneutical_control_is_the_true_locus_of_post_conciliar_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_the_true_locus_of_post_conciliar_authority, holdable).
narrative_ontology:cs_axiom_grounding('10513884-2eb1-477c-ad7e-4cf13870c27a', hermeneutical_control_is_the_true_locus_of_post_conciliar_authority, conventional).
narrative_ontology:cs_axiom('10513884-2eb1-477c-ad7e-4cf13870c27a', secondary, implementation_divergence_is_structural_not_accidental).
narrative_ontology:cs_axiom_status(implementation_divergence_is_structural_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('10513884-2eb1-477c-ad7e-4cf13870c27a', implementation_divergence_is_structural_not_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('10513884-2eb1-477c-ad7e-4cf13870c27a', engineered_compromise_at_ratification).
narrative_ontology:cs_drift_state('10513884-2eb1-477c-ad7e-4cf13870c27a', contemporary_post_conciliar_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('10513884-2eb1-477c-ad7e-4cf13870c27a', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_office).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, national_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theological_faculties).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_movements).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the authoritative interpretation of ambiguous conciliar formulations (e.g. collegiality vs. papal primacy, subsistit in, religious liberty vs. prior condemnations). Because the texts themselves do not resolve which vision governs, whoever controls interpretation controls what the Council 'actually said.' This office issues clarifying instructions, approves catechisms, and adjudicates disputes between competing readings — a form of authority the compromise drafting itself created.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_office, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Gained a structural warrant for regional pastoral adaptation from the ambiguity around collegiality. Different conferences implement liturgy, catechesis, and discipline according to whichever reading of the compromise texts suits local conditions, producing durable divergence between, e.g., dioceses that read the Council as continuity and those that read it as reform — divergence the texts permit rather than resolve.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, national_episcopal_conferences, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, national_episcopal_conferences, agenda_setter).

% Built entire theological programs and academic careers on contested readings of the ambiguous formulations. The permanent unresolved status of the texts generates ongoing scholarly output, conferences, and institutional positions — resolution would collapse a productive interpretive economy that currently sustains journals, chairs, and doctoral pipelines.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_theological_faculties, beneficiary,
    organized, generational, mobile, global).

% Read the ambiguous texts as encoding genuine rupture with prior magisterium and experience the compromise formulations as having been used, in practice, to displace the liturgical and doctrinal forms they hold to be normative. Roughly 10-12% at the founding vote refused ratification outright; their descendants remain in canonically marginal or irregular status, unable to secure an authoritative ruling that the ambiguity itself was ever acknowledged as such rather than settled against them.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    powerless, generational, trapped, national).

% Read the same ambiguous formulations as mandating a genuine break — collegial governance, expanded lay role, ecumenical openness — and experience repeated re-assertion of the continuity reading by central authority as the compromise being resolved against them after the fact, decades after ratification, without the ambiguity being named as ambiguity at the time of resolution.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_movements, payer,
    moderate, generational, constrained, global).

% Live with whichever local implementation their diocese or parish happens to have adopted, often without any visibility into the fact that the governing texts are compromise formulations rather than settled doctrine. Bear the practical cost of doctrinal and liturgical instability (parish closures, catechetical incoherence, generational rupture in practice) generated by a dispute they did not create and cannot adjudicate.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful, payer,
    powerless, biographical, constrained, local).

% The theologians who drafted the ambiguous compromise language to secure supermajority passage are mostly deceased or long removed from active influence; their private correspondence and drafting notes (where preserved) often show they knew the formulations were deliberately underdetermined to secure votes from both conservative and reformist blocs. Their testimony to intentional ambiguity is available only in archives, not in live ecclesial deliberation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_periti_and_drafting_commissions, excluded,
    moderate, biographical, trapped, global).

% Study conciliar voting records, drafting history, and the documented rejection minority to establish that the final texts were engineered compromises rather than univocal statements. Their work is cited by all factions selectively but adopted authoritatively by none, since acknowledging deliberate overdetermination would destabilize whichever reading currently holds institutional power.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The compromise drafting process solved a genuine and urgent problem: assembling a supermajority of bishops with sharply divergent theological commitments (neo-scholastic continuity bishops vs. reform-minded bishops influenced by the nouvelle theologie) into texts all factions could ratify without schism. Ambiguous formulations were the coordination mechanism that averted an immediate institutional rupture in 1962-65.
% TRANSFER_FUNCTION: The arrangement transfers interpretive authority from the plain text of the documents themselves to whichever body currently controls magisterial interpretation — moving practical doctrinal and liturgical certainty away from ordinary faithful and dissenting minorities and toward central hermeneutical offices and the academic-episcopal apparatus that produces and adjudicates readings.
% ABSENT_VOICES: The traditionalist minority that voted against ratification (roughly 10-12% on contested schemas) and their theological heirs are treated as having 'lost' a settled vote rather than as having correctly identified an unresolved incompatibility in the text; their objection is structurally excluded from being heard as a diagnosis of the text rather than a failure of assent. The original drafters who could attest to intentional ambiguity are mostly unavailable to living deliberation.
% DISAPPEARANCE_RATIONALE: If the overdetermined composite status of the texts were formally acknowledged by central authority — rather than adjudicated as if the texts had one settled meaning — the entire structure of hermeneutical control (who gets to say what the Council 'really meant') would lose its object. Competing schools would have to argue directly about which ecclesiology is theologically correct rather than about which reading the ambiguous text supports, collapsing a major axis of institutional authority currently exercised through interpretation.
% FOUNDING_PROBLEM: The Council needed to produce documents that neo-scholastic continuity-minded bishops and reform-minded bishops influenced by ressourcement theology could both vote to ratify, in order to avoid an open schism at the Council itself and preserve institutional unity during a period of acute internal theological conflict.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the Council (drawing on the drafting commission records, the documented history of successive schema revisions, and the recorded near-unanimous but not unanimous final votes with substantial minority rejections on contested schemas) attest that the ambiguity was a deliberate drafting strategy, not incidental vagueness — this is corroborated by archival material from outside any single ecclesiological faction, though the central hermeneutical authority itself has never formally acknowledged the texts as intentionally overdetermined rather than settled.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at 2025) reflects the substantial ongoing institutional benefit that hermeneutical control, regional adaptation latitude, and academic production draw from perpetuated textual ambiguity, but it is not extreme because a genuine coordination function (averting 1960s-era schism) really was served and continues to have partial residual value in preserving overall ecclesial unity. Suppression (0.44) is moderate: dissenting readings are not violently suppressed but are structurally denied authoritative standing — the central hermeneutical office can decline to acknowledge the ambiguity as ambiguity, which functions as a soft suppression of the diagnosis itself. Accessibility collapse is low-moderate (0.35): once one recognizes the composite-overdetermination structure, alternative readings of the same texts remain fully available in the documentary record — this is not a case where alternatives disappear, which is part of why this reading itself remains contestable rather than self-evidently correct. Resistance is high (0.68): both traditionalist and progressive factions actively resist the hermeneutical status quo, each wanting a different official resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the curial hermeneutical office's seat, the compromise formulations look like living tradition requiring careful ongoing interpretation — a genuine pastoral and theological task. From the traditionalist and progressive payer seats, the same textual structure looks like an unresolved fracture being adjudicated, decades later, in favor of whichever faction currently holds interpretive power — extraction dressed as development. The engine should compute these divergently from the same structural facts; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The curial hermeneutical office sits nearest the beneficiary end: it does not create the ambiguity but administers it, and the authority to adjudicate what the Council 'really meant' is a power the ambiguity itself generates. National episcopal conferences and theological faculties are secondary beneficiaries — they did not design the ambiguity but have built durable institutional practice and scholarship on exploiting it. Traditionalist communities, progressive reform movements, and ordinary parish faithful are targets: each bears real costs from the persistence of unresolved ambiguity, though for structurally different reasons (traditionalists experience de facto rupture being imposed; progressives experience de facto continuity being reasserted after initial reformist momentum; ordinary faithful bear generalized instability without a coherent narrative of why). The excluded drafters occupy a distinct position — they possess evidence of the intentional design but no live voice in the current dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (averting 1960s schism via engineered ambiguity) is best read as partially live and partially dead: the underlying theological divide the ambiguity was built to paper over has not been resolved (live), but the specific crisis-avoidance function that justified deliberate textual underdetermination in 1965 no longer requires ongoing ambiguity sixty years later — the Church is not on the verge of the same schism, yet the interpretive economy built around unresolved ambiguity persists and now serves institutional actors (hermeneutical office, academic faculties) whose interest lies in non-resolution. This mismatch between a founding crisis that has receded and a hermeneutical apparatus that persists and benefits from non-resolution is exactly the tangled-rope signature: it prevents the mistake of calling the whole arrangement pure extraction (a real coordination problem was genuinely solved at the founding) while also preventing the mistake of calling it pure coordination now (parties currently pay ongoing costs through the same ambiguity-preserving structure that once saved the institution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_versus_emergent_ambiguity,
    'Was the textual ambiguity in the conciliar documents a deliberate drafting strategy by the commissions (as private correspondence and drafting-history scholarship suggests for several key schemas), or did it emerge from ordinary compromise pressure without conscious design to encode both readings?',
    'Systematic archival review of drafting commission minutes, successive schema revisions, and periti correspondence across all sixteen conciliar documents, comparing degree of textual ambiguity against documented awareness of the underlying theological conflict at time of drafting.',
    'If ambiguity is shown to be substantially intentional across multiple key documents, the composite-overdetermination reading is strongly corroborated as a description of the drafting process itself, not merely of later reception. If ambiguity is shown to be largely an artifact of ordinary multi-author compromise without conscious dual-encoding, the reading remains viable as a description of present function but loses its strongest historical warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_versus_emergent_ambiguity, empirical, 'Whether the ambiguity in the final texts was designed or emergent.').

omega_variable(
    hermeneutical_authority_as_genuine_versus_extractive,
    'Is the central hermeneutical office''s exercise of interpretive authority over ambiguous texts a legitimate exercise of ordinary magisterial function (courts interpret ambiguous statutes; this is not novel), or is it an extractive capture of authority that the ambiguity itself manufactured and that would not otherwise exist?',
    'Comparative analysis against how magisterial interpretation has historically operated on unambiguous conciliar texts versus ambiguous ones, and whether the volume and consequence of interpretive rulings issued specifically to resolve Vatican II ambiguities exceeds the historical baseline for ordinary doctrinal development.',
    'If ordinary, the tangled_rope classification overstates extraction and the arrangement is closer to a legitimate, if imperfect, rope. If manufactured, the tangled_rope reading is corroborated and the hermeneutical office''s authority is substantially a product of the drafting compromise rather than an independent magisterial competence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_authority_as_genuine_versus_extractive, conceptual, 'Whether interpretive authority over the ambiguous texts is ordinary magisterial function or manufactured extraction.').

omega_variable(
    cs_framing_kernel_versus_authority_layer,
    'Should the kernel for this reading be framed as the conciliar texts themselves (a fixed_text kernel under lineage authority), or as the layered legitimacy claim that magisterial continuity requires — i.e., the doctrine that whatever the Magisterium currently says the Council meant IS what it meant, independent of textual content?',
    'Distinguish cases where the hermeneutical office cites the text directly (fixed_text framing) from cases where it cites its own prior interpretive rulings as the operative authority (self-referential legitimacy-layer framing), and track which framing predominates in actual doctrinal practice over the interval.',
    'Under the fixed_text framing, this constraint''s cs_structure correctly names the texts as kernel and the curial office as lineage-grounded interpreter. Under the legitimacy-layer framing, the true kernel is the doctrine of magisterial interpretive infallibility itself, and this constraint would be better modeled as one layer within a larger commitment system whose kernel is that second-order doctrine — a materially different classification with different foreclosure dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_authority_layer, conceptual, 'Whether the kernel is the conciliar text itself or the second-order doctrine of magisterial interpretive authority over it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.4).
narrative_ontology:measurement(vati_tr_t2001, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2001, 0.46).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.5).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.47).
narrative_ontology:measurement(vati_be_t2001, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2001, 0.51).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.55).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.36).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement(vati_su_t2001, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.42).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vatican_ii_magisterial_authority kernel. continuity_reading holds the conciliar texts represent organic development within unbroken tradition (likely mountain-leaning, low extraction, high accessibility collapse from that reading's own premises). rupture_reading holds the texts encode a fundamental break incompatible with prior magisterium (likely snare or tangled_rope-leaning from a traditionalist vantage, with different victim sets than this story). This composite_overdetermination_reading differs from both by denying univocity altogether: rather than adjudicating which single meaning the texts have, it holds the texts were engineered to support both simultaneously, relocating the true site of authority to the interpretive apparatus itself. Each reading authors its own ε against the same underlying textual corpus but from a different premise about what kind of object the corpus is (settled development, settled rupture, or unsettled composite) — per the ε-invariance principle these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
