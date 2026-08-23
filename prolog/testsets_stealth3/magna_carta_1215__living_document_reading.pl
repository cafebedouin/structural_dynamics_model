% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate — the Interpretive-Tradition Constraint
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This story classifies the constraint instantiated by the living-document
 *   reading of Magna Carta: the standing arrangement under which the
 *   charter's constitutional authority is carried by an eight-century
 *   accumulated interpretive tradition rather than by its 1215 terms. Under
 *   this arrangement, original meaning is legitimately superseded — Coke's
 *   transformation of clause 39 into due process, the tradition's crossing
 *   into written constitutions, and the modern precedent stock are not
 *   deviations from the charter but its continuing substance. The constraint
 *   binds interpreters to that accumulation: precedent governs, and the
 *   accumulation itself constitutes constitutional development. Its operation
 *   has a real coordination face (a fixed text governs transformed
 *   circumstances without rupture or perpetual re-enactment) and a real
 *   transfer face (effective lawmaking authority over constitutional
 *   questions has moved from enacted text and elected bodies to the
 *   interpreter class that administers the accumulation). The claimed type
 *   and the metrics are authored independently: the claim states what this
 *   reading takes the structure to be; the metrics describe how the
 *   arrangement actually operates. ε is authored for the standing
 *   living-tradition arrangement, assessed by this reading's own lights. KEY
 *   AGENTS (by structural relationship): - appellate_judiciary: agenda-setter
 *   and principal collector (institutional / identity_locked) — administers
 *   the accumulation, adds to it, draws lawmaking effect from it -
 *   legal_profession: primary beneficiary (organized / constrained) —
 *   collects fees and status from mastery of the tradition -
 *   constitutional_law_academy: secondary beneficiary (moderate /
 *   identity_locked) — careers fused with the interpretive project -
 *   democratic_majorities: principal payer (organized / constrained) — bound
 *   by evolved meanings they never enacted - elected_legislatures: payer
 *   (institutional / constrained) — can pass laws but not meanings -
 *   originalist_interpreters: payer (moderate / constrained) — methodology
 *   institutionally subordinated to binding precedent -
 *   unrepresented_lay_citizens: excluded (powerless / trapped) — no seat in
 *   the forum where meaning is made - constitutional_historians: analytical
 *   observer (analytical / analytical) — sees original terms and accumulation
 *   together
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.7).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate — the Interpretive-Tradition Constraint").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '3c251b33-cd91-4796-adc3-1641602d6870').
narrative_ontology:cs_kernel_codification('3c251b33-cd91-4796-adc3-1641602d6870', fixed_text).
narrative_ontology:cs_authority_grounding('3c251b33-cd91-4796-adc3-1641602d6870', lineage).
narrative_ontology:cs_interpretation_layer_present('3c251b33-cd91-4796-adc3-1641602d6870').
narrative_ontology:cs_reading_relation('3c251b33-cd91-4796-adc3-1641602d6870', magna_carta_1215__baronial_privilege_reading, influences).
narrative_ontology:cs_reading_relation('3c251b33-cd91-4796-adc3-1641602d6870', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('3c251b33-cd91-4796-adc3-1641602d6870', foundational, original_meaning_legitimately_superseded).
narrative_ontology:cs_axiom_status(original_meaning_legitimately_superseded, holdable).
narrative_ontology:cs_axiom_grounding('3c251b33-cd91-4796-adc3-1641602d6870', original_meaning_legitimately_superseded, conventional).
narrative_ontology:cs_axiom('3c251b33-cd91-4796-adc3-1641602d6870', foundational, precedential_accumulation_constitutes_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('3c251b33-cd91-4796-adc3-1641602d6870', precedential_accumulation_constitutes_development, instrumental).
narrative_ontology:cs_reference_frame('3c251b33-cd91-4796-adc3-1641602d6870', adaptive_constitutional_substrate).
narrative_ontology:cs_drift_state('3c251b33-cd91-4796-adc3-1641602d6870', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3c251b33-cd91-4796-adc3-1641602d6870', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, appellate_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_law_academy).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, democratic_majorities).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, elected_legislatures).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_interpreters).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, stare_decisis_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_accumulation_principle).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, interpretive_tradition_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges on apex and intermediate appellate courts decide which earlier readings bind, how far they reach, and when they yield. Each generation inherits the accumulated readings and adds to them; judicial authority is exercised through and constituted by command of that inheritance. They administer the accumulation — docket control, doctrinal tests, the overruling power — and their lawmaking effect grows with each precedent they add. Leaving the bench does not leave the tradition: a former judge's standing rests on it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, appellate_judiciary, beneficiary).

% Attorneys earn their living navigating the accumulated readings; mastery of eight centuries of interpretation is the profession's barrier to entry and the basis of its fees, status, and self-reproduction through law schools. They collect from the arrangement without setting it. Exit would mean abandoning legal practice as currently constituted.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Scholars whose careers, reputations, and institutional positions are built on interpreting, criticizing, and extending the accumulated tradition. Their scholarly identity is fused with the interpretive project; the tradition's persistence is the persistence of the object of their life's work.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_law_academy, beneficiary,
    moderate, biographical, identity_locked, national).

% Citizens acting as voting majorities find their enacted policies bounded by constitutional meanings that shifted through judicial accumulation rather than enactment. Their formal exits — constitutional amendment, electing the officials who appoint judges — exist but are supermajoritarian, slow, and rarely reach the specific doctrines that bind them. They carry evolved obligations and prohibitions they never voted on.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, democratic_majorities, payer,
    organized, generational, constrained, national).

% Legislatures draft statutes that courts construe through the accumulated tradition; their products are narrowed, extended, or struck under meanings no legislature enacted. They hold real institutional power — they can pass laws and, with supermajorities, amend the text — but they cannot fix meanings; interpretation is not theirs.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, elected_legislatures, payer,
    institutional, immediate, constrained, national).

% Judges, scholars, and advocates who decide or argue cases by the text's original meaning. Inside the courts their method is institutionally subordinate to binding precedent: they may dissent and write, but must apply accumulated readings they disavow. Their resistance is organized and well-funded yet holds no agenda-setting power over which readings accumulate.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_interpreters, payer,
    moderate, biographical, constrained, national).

% Citizens without legal training have no seat in the forums where constitutional meaning is made — argument runs in professional idiom through courts, law reviews, and clerkships. They meet the tradition only as commands and prohibitions. Their objection, that the meaning of their constitution is settled without them, is never voiced where it would count.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, unrepresented_lay_citizens, excluded,
    powerless, biographical, trapped, national).

% Historians of the charter and its reception who can hold the 1215 terms and the eight-century accumulation in view at once. They document which clauses died, when the tradition took over the charter's authority, and what each era's reading did to the last. They collect nothing from the arrangement and decide nothing in it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, appellate_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a fixed thirteenth-century text can continue to govern a polity whose circumstances, institutions, and moral commitments have repeatedly transformed: change is routed through accumulated precedent instead of through constitutional rupture or perpetual re-enactment, and the interpretive community coordinates on a single evolving meaning rather than fragmenting into private readings.
% TRANSFER_FUNCTION: Moves effective lawmaking authority over constitutional questions from enacted text and democratic majorities to the accumulating judicial tradition and the professional class that administers it; incidentally moves fees, status, and careers to those who master the accumulation.
% ABSENT_VOICES: The original contracting parties and each superseded founding generation are structurally absent — dead, with no advocate holding agenda power; their superseded meaning is argued only by proxy, since originalist interpreters are present in the forums but hold no agenda-setting power over the accumulation. Lay citizens without legal training are absent from the forum where meaning is made entirely. The interpretive community's practical unanimity around tradition-governance is therefore partly an artifact of who is in the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if accumulated readings stopped binding and original meaning governed — settled doctrine across due process, liberty, and governmental structure would collapse into open contest, thousands of precedents would lose force at once, the profession's authority structure would dissolve, and constitutional conflict would migrate from courts to raw political struggle. The arrangement is load-bearing.
% FOUNDING_PROBLEM: By the seventeenth century the charter's original feudal terms — wardship, reliefs, scutage, the baronial security clause — had become inoperative or repugnant, yet the charter's authority was invoked more strongly than ever. The problem: what makes a thirteenth-century text continue to bind and legitimate a modern order? The living-document answer: the accumulated tradition, not the original terms, carries the authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties. Constitutional historians — the analytical seat, collecting nothing — document that the charter's operative force survived the death of its original subject matter only through successive re-readings (Coke's transformation of clause 39 into due process; the tradition's crossing into written constitutions; the modern precedent stock). Adversarially, originalist scholars corroborate the same historical facts while disputing their legitimacy. Comparative evidence: contemporaneous charters without a continuous interpretive tradition, such as the Hungarian Golden Bull of 1222, faded into archival irrelevance — corroborating that the tradition, not the text, did the carrying.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) reflects a real and growing transfer: effective lawmaking authority over constitutional questions has moved from enacted text and elected bodies to the accumulating judicial tradition and the professional class that administers it, while the tradition simultaneously delivers the genuine coordination good of continuity-with-adaptation. Suppression (0.70) is institutional rather than discursive: stare decisis is actively enforced — lower courts are bound, deviations are reversed, hierarchical discipline operates — yet originalist methodology remains fully articulable in dissent, scholarship, and politics, so alternatives persist rather than collapse (accessibility_collapse 0.30). Resistance (0.60) is correspondingly high: an organized, well-funded counter-movement contests the tradition's authority. Theater (0.35) is moderate: the precedent stock does real decisional work, but as the charter's specific clauses died (only clauses 1, 13, 39, and 40 remain on the English statute book) the share of ceremonial veneration — anniversaries, rhetorical invocation in opinions that decide nothing — grew. The three series share one time grid (1215, 1400, 1608, 1689, 1787, 1965, 2025). Extractiveness and enforcement capacity rise together as the interpretive apparatus institutionalizes (Coke's assertion of judicial interpretive authority against the crown, then judicial review, then full modern stare decisis); the late dip (1965→2025) reflects the originalist counter-movement's partial success in narrowing doctrines and shifting appointments, not a reversal of accumulation. Receipt-surface facts: the gains demonstrably accrue to the appellate bench — its lawmaking effect compounds with each precedent added — and wholesale fixing is prohibitive for the only actor with the power to attempt it, since overruling the accumulated stock would destabilize thousands of settled positions and the court's own legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the bench, the tradition IS the constitution: continuity, craft, fidelity-to-practice; the accumulation is the institution's whole mode of being, and a judge's professional self is constituted inside it. From democratic_majorities and elected_legislatures, the same structure operates as lawmaking without enactment — binding them to meanings no one they elected chose. Originalist_interpreters occupy a third position: full membership in the profession, institutional subordination of their method — they may dissent and write but must apply readings they disavow. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the bench administers the accumulation and its authority compounds with it (d near the beneficiary end, amplified by identity-locked exit — leaving the bench does not leave the tradition); the profession and academy collect fees, status, and careers (low d). Payers: democratic_majorities bear evolved obligations with only supermajoritarian, slow exits (high d); elected_legislatures hold institutional power but no interpretive authority (high d); originalist_interpreters bear the supersession of their method itself — the thing taken is their professional practice (high d). Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is what the engine scales through directionality and scope. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement is routinely mislabeled from both flanks: its beneficiaries describe pure coordination (continuity, the rule of law, peaceful adaptation) and its payers describe pure extraction (judicial usurpation, taxation of meaning). The hybrid structure holds both: a real coordination function — routing constitutional change through accumulated precedent instead of rupture or perpetual re-enactment — operating through the same structure that transfers lawmaking authority to an identity-locked interpreter class. On obsolescence: the founding problem (an obsolete text must still bind) is live and regenerates each generation, so no sunset is due; the arrangement's function was transformed, not exhausted, and the transformation is the function. The genealogy fields record this — founding_problem_status live, disappearance verdict world_rearranges — so the dead-mandate mismatch flag should not fire: the arrangement's function is genuinely current even though its original subject matter (feudal tenures) died centuries ago.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the magna_carta_1215 kernel governs: baronial_privilege_reading (original terms bind; protections run only to contracting landowners), this living_document_reading (accumulated tradition legitimately supersedes original meaning), or universal_rights_reading (clause 39 emits a transhistorical due-process constraint on all persons)?',
    'Not resolvable by data alone: the readings disagree about the source of the charter''s binding authority. Resolution tracks which reading captures the operative interpretive institutions over time — appointment patterns, whether original-meaning arguments displace tradition-based ones in controlling opinions, and whether the accumulation continues or is pruned.',
    'The baronial reading would dissolve this constraint''s modern beneficiary structure entirely (a dead feudal contract has no living interpreter class to collect) and collapse the profile toward a historical artifact; the universal_rights reading would change the victim set (clause 39 binds universally regardless of the tradition) and re-author extraction around rights-dilution rather than authority-transfer. Classification is per-reading; the readings must not be averaged into one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which reading of the charter kernel governs, and where the disagreement is structurally located (referent of ''free men'', force of original terms, authority of accumulation).').

omega_variable(
    cross_reading_epsilon_divergence,
    'ε is reading-indexed: this story authors the living reading''s ε over the standing living-tradition arrangement. What ε would each sibling reading author over the same referent, and does the corpus keep them separate?',
    'Author the sibling stories and compare: the baronial reading should author near-total extraction (the entire tradition is usurpation of the feudal contract); the universal reading a different profile (the tradition as both vehicle and diluter of universal due process).',
    'If sibling ε values are averaged or merged, the family''s classification is meaningless; each reading''s constraint must carry its own type. This omega guards the ε-invariance boundary of the magna_carta_1215 family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_reading_epsilon_divergence, conceptual, 'Reading-indexed ε divergence across the magna_carta_1215 constraint family.').

omega_variable(
    interpreter_capture_vs_development,
    'Is the transfer of lawmaking authority to the interpreter class a coordination cost of continuity-with-adaptation, or capture — does the accumulated tradition track public constitutional commitments or the interpreter class''s institutional interests?',
    'Compare doctrinal outcomes against both democratic preferences and professional interests across eras and jurisdictions; natural experiments where appointment regimes shifted abruptly (e.g., post-1970 United States appointment changes) reveal whether accumulation follows the bench''s composition or an independent developmental logic.',
    'If capture dominates, the constraint slides toward pure extraction — the coordination story becomes cover for class rule; if development dominates, the measured extraction is largely the price of the coordination itself and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpreter_capture_vs_development, empirical, 'Coordination cost versus interpreter-class capture in precedential accumulation.').

omega_variable(
    precedent_binding_mechanism,
    'What holds the constraint on individual interpreters — structural enforcement (stare decisis as institutional rule, reversal risk, hierarchical discipline) or internalized professional identity (judges who disavow precedent still apply it because the judicial role is fused with the tradition)?',
    'Track the behavior of methodologically originalist judges confronting disfavored binding precedent: sustained application of precedent they disavow indicates a strong internalized component; opportunistic overruling once a majority forms indicates structural dominance.',
    'If internalized, individual exit cannot erode the constraint and the profession carries its own binding force; if structural, doctrinal majorities can break it quickly and the enforcement trajectory dominates the persistence question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_binding_mechanism, empirical, 'Structural versus internalized source of precedent''s binding force on interpreters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__living_document_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement_basis(magn_tr_t1400, observed).
narrative_ontology:measurement(magn_tr_t1608, magna_carta_1215__living_document_reading, theater_ratio, 1608, 0.24).
narrative_ontology:measurement_basis(magn_tr_t1608, observed).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__living_document_reading, theater_ratio, 1689, 0.27).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1787, magna_carta_1215__living_document_reading, theater_ratio, 1787, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1787, observed).
narrative_ontology:measurement(magn_tr_t1965, magna_carta_1215__living_document_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement_basis(magn_tr_t1965, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__living_document_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.12).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__living_document_reading, base_extractiveness, 1400, 0.24).
narrative_ontology:measurement_basis(magn_be_t1400, observed).
narrative_ontology:measurement(magn_be_t1608, magna_carta_1215__living_document_reading, base_extractiveness, 1608, 0.38).
narrative_ontology:measurement_basis(magn_be_t1608, observed).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__living_document_reading, base_extractiveness, 1689, 0.43).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1787, magna_carta_1215__living_document_reading, base_extractiveness, 1787, 0.5).
narrative_ontology:measurement_basis(magn_be_t1787, observed).
narrative_ontology:measurement(magn_be_t1965, magna_carta_1215__living_document_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement_basis(magn_be_t1965, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__living_document_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.08).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__living_document_reading, suppression_requirement, 1400, 0.25).
narrative_ontology:measurement_basis(magn_su_t1400, observed).
narrative_ontology:measurement(magn_su_t1608, magna_carta_1215__living_document_reading, suppression_requirement, 1608, 0.45).
narrative_ontology:measurement_basis(magn_su_t1608, observed).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__living_document_reading, suppression_requirement, 1689, 0.52).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1787, magna_carta_1215__living_document_reading, suppression_requirement, 1787, 0.6).
narrative_ontology:measurement_basis(magn_su_t1787, observed).
narrative_ontology:measurement(magn_su_t1965, magna_carta_1215__living_document_reading, suppression_requirement, 1965, 0.76).
narrative_ontology:measurement_basis(magn_su_t1965, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_1215__living_document_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Magna Carta' decomposes, per ε-invariance, into three structurally distinct constraints — one per reading of the magna_carta_1215 kernel. The baronial_privilege_reading classifies a dead feudal contract (ε near the historical-artifact profile; no living beneficiary class). The universal_rights_reading classifies a transhistorical due-process constraint emitted by clause 39 (victim set: all persons denied due process; the tradition is at most a vehicle). This story, the living_document_reading, classifies the interpretive-tradition arrangement itself (ε: transfer of lawmaking authority from enacted text and majorities to the interpreter class). Each story carries its own ε, beneficiaries, and victims; the edges here record the family linkage required by the decomposition, not a claim that the three subject matters causally drive one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
