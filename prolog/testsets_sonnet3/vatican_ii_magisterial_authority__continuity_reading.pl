% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity — Vatican II as Organic Development
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story authors the CONTINUITY reading of the contested Vatican II
 *   magisterial-authority kernel: the claim that the Council's texts
 *   represent organic development within unbroken tradition, that no prior
 *   magisterial teaching was overturned, that 'spirit of Vatican II'
 *   expansive applications are unauthorized extrapolations beyond the actual
 *   texts, that the Latin-preservation mandate of Sacrosanctum Concilium §36
 *   remains binding law rather than a dead letter, and that Dignitatis
 *   Humanae's religious liberty teaching is reconcilable with the Syllabus of
 *   Errors via a thesis/hypothesis distinction or a doctrinal-development
 *   framework (per Newman-derived theory as deployed by Ratzinger/Benedict
 *   XVI). This reading is authored as ONE constraint among three siblings
 *   sharing the same kernel — the rupture reading and the
 *   composite-overdetermination reading are separate constraints with their
 *   own ε values, not alternate measurements of this one. The referent for
 *   extractiveness here is the standing arrangement AS THE CONTINUITY READING
 *   ITSELF SEES IT: an authoritative interpretive discipline that, in its own
 *   telling, preserves ecclesial unity but that this authoring seat
 *   nonetheless observes extracting real costs from those whose readings it
 *   forecloses.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium_office: agenda_setter (institutional/analytical) — issues and enforces the continuity hermeneutic
 *   - curial_continuity_faculty: beneficiary (institutional/constrained) — professional and doctrinal stake in continuity being correct
 *   - traditionalist_leaning_bishops: beneficiary/payer (powerful/constrained) — use continuity to discipline pastoral overreach, but are bound by it to accept conciliar teaching wholesale
 *   - progressive_pastoral_reformers: payer (moderate/constrained) — 'spirit of Vatican II' appeals delegitimized
 *   - vernacular_liturgy_advocates: payer (moderate/constrained) — Latin-preservation clause invoked against vernacular practice
 *   - traditionalist_separatist_communities: payer (powerless/trapped) — their rupture claim is declared a factual error rather than a defensible judgment
 *   - academic_ecclesiastical_historians: observer (analytical) — assess drafting history independent of institutional stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.42).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Hermeneutic of Continuity — Vatican II as Organic Development").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '58cdac2d-2e45-436b-93af-cb4dd62ba2dc').
narrative_ontology:cs_kernel_codification('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', fixed_text).
narrative_ontology:cs_authority_grounding('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', lineage).
narrative_ontology:cs_interpretation_layer_present('58cdac2d-2e45-436b-93af-cb4dd62ba2dc').
narrative_ontology:cs_reading_relation('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', foundational, conciliar_texts_bind_to_preserve_prior_doctrine).
narrative_ontology:cs_axiom_status(conciliar_texts_bind_to_preserve_prior_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', conciliar_texts_bind_to_preserve_prior_doctrine, conventional).
narrative_ontology:cs_axiom('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', foundational, spirit_of_council_claims_lack_magisterial_authority).
narrative_ontology:cs_axiom_status(spirit_of_council_claims_lack_magisterial_authority, holdable).
narrative_ontology:cs_axiom_grounding('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', spirit_of_council_claims_lack_magisterial_authority, conventional).
narrative_ontology:cs_axiom('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', secondary, religious_liberty_reconcilable_via_thesis_hypothesis_development).
narrative_ontology:cs_axiom_status(religious_liberty_reconcilable_via_thesis_hypothesis_development, holdable).
narrative_ontology:cs_axiom_grounding('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', religious_liberty_reconcilable_via_thesis_hypothesis_development, instrumental).
narrative_ontology:cs_reference_frame('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', unbroken_apostolic_magisterial_tradition).
narrative_ontology:cs_drift_state('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', post_benedict_xvi_hermeneutic_address, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('58cdac2d-2e45-436b-93af-cb4dd62ba2dc', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, curial_continuity_faculty).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_magisterium_office).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_pastoral_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgy_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_separatist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues authoritative interpretive documents (e.g. Benedict XVI's 2005 Curia address articulating the 'hermeneutic of reform in continuity') that adjudicate which readings of conciliar texts are licit. Controls seminary formation, doctrinal congregations, and disciplinary proceedings against readings it deems ruptures. Collects legitimacy from the claim that no break occurred.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_magisterium_office, agenda_setter,
    institutional, civilizational, analytical, global).

% Theology faculties and curial offices whose careers and institutional standing depend on demonstrating textual and doctrinal continuity across the conciliar threshold. Produce the scholarship that grounds thesis/hypothesis reconciliation arguments (e.g. Dignitatis Humanae read against the Syllabus). Benefit from continuity being the settled frame because it validates their interpretive labor and forecloses competing schools.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, curial_continuity_faculty, beneficiary,
    institutional, generational, constrained, global).

% Bishops who accept the Council's legitimacy but resist expansive pastoral applications (vernacular maximalism, liturgical improvisation) by invoking continuity doctrine to discipline implementation. Benefit from the reading's constraint on 'spirit of Vatican II' claims, but also pay a cost: they cannot simply reject conciliar teaching outright, since continuity binds them to accept it as authentically magisterial, foreclosing a cleaner traditionalist rejection.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops, payer).

% Clergy and lay reformers who read the Council's pastoral orientation (collegiality, religious liberty, ecumenism, liturgical vernacularization) as licensing substantial doctrinal and structural evolution. Under the continuity reading, their 'spirit of Vatican II' appeals are declared unauthorized extensions beyond the text, and their reform initiatives can be disciplined or defunded by diocesan authority invoking magisterial fidelity. They cannot easily exit — their vocation and ecclesial standing are inside the same institution that adjudicates the reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_pastoral_reformers, payer,
    moderate, biographical, constrained, regional).

% Parishes and liturgists who pushed vernacular and inculturated liturgical practice as the Council's evident trajectory. The continuity reading's insistence that Sacrosanctum Concilium §36's Latin-preservation clause remains binding recharacterizes much post-conciliar vernacular practice as an unauthorized overreach rather than a fulfillment of conciliar intent, exposing their practice to correction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgy_advocates, payer,
    moderate, biographical, constrained, regional).

% Groups (e.g. communities aligned with the wider SSPX-adjacent milieu) who argue the Council effected genuine rupture and refuse full communion on that basis. The continuity reading directly delegitimizes their founding claim: by insisting no rupture occurred, it frames their separation as based on a factual error about the Council rather than a defensible judgment, closing off recognition of their objection as anything but disobedience. Exit from the Church's institutional structure costs them recognized sacramental standing.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_separatist_communities, payer,
    powerless, generational, trapped, regional).

% Historians and theologians outside the disciplinary chain who examine conciliar drafting history, the acta of the Council, and periti correspondence to assess whether textual compromises encode genuine doctrinal shifts or surface-level continuity. Their findings are cited by all factions but bind none institutionally.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, academic_ecclesiastical_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_magisterium_office).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive frame that lets the institution treat the entire conciliar corpus as binding without requiring any bishop, theologian, or diocese to adjudicate case-by-case whether a given conciliar teaching overturned a prior one — continuity is asserted as a governing premise rather than litigated document by document, which lets formation, catechesis, and canon law proceed on a stable footing.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward the central magisterium and its aligned faculties, and moves the cost of doctrinal ambiguity onto whichever local actors (progressive reformers, vernacular advocates, traditionalist separatists) read the Council as licensing or requiring change the continuity frame does not authorize. Disciplinary and reputational costs are transferred to those readings; interpretive certainty and institutional cohesion accrue to the center.
% ABSENT_VOICES: The conciliar periti and drafting committees who negotiated the ambiguous compromise language (e.g. on collegiality, religious liberty) are largely dead or unheard from directly; their private correspondence and diaries (partially available to historians) sometimes reveal drafting intentions in tension with the continuity gloss, but this evidence is filtered through academic rather than magisterial channels and does not bind interpretation.
% DISAPPEARANCE_RATIONALE: If the continuity frame were officially abandoned, disciplinary actions currently grounded in it (against both progressive over-implementation and traditionalist rejection) would lose their doctrinal warrant; seminary formation curricula built on reform-in-continuity theology would require revision; and the Church's self-understanding of unbroken apostolic succession through the conciliar threshold would become an open, rather than settled, question with direct implications for the legitimacy of ordinations, liturgical norms, and ecumenical agreements made under the conciliar dispensation.
% FOUNDING_PROBLEM: The Council itself, and its immediate aftermath, produced genuinely ambiguous texts (compromise formulations reconciling competing conciliar factions) that admitted multiple readings; the continuity reading was developed to resolve this ambiguity in a way that preserved institutional and doctrinal stability by denying that any of the readings constituted rupture.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium (via Benedict XVI's 2005 address and subsequent curial statements) attests the founding problem — ambiguity risking rupture — was real and has been correctly resolved by the continuity hermeneutic. Independent academic historians (e.g. the Bologna School and its critics) dispute this from outside the beneficiary set: some corroborate that continuity is a defensible textual reading, others argue the ambiguity was never resolved but merely declared resolved by fiat, and traditionalist separatist communities corroborate that a problem persists precisely because they were never convinced continuity holds. No fully disinterested corroborating source exists — every attesting party has a stake in the outcome, including the historians, whose scholarly reputations often track pre-existing sympathies toward one reading or another.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate rather than low: the continuity reading does perform real coordinating work (a single interpretive key across a a vast and doctrinally sensitive corpus) but that coordination is achieved partly by declaring certain readings illegitimate rather than by winning the argument on textual grounds alone, which is where the extractive component lives — costs are imposed on separatist and reformist readings without those readings being refuted so much as administratively foreclosed. Suppression (0.55) reflects real disciplinary machinery: canonical processes, removal from teaching posts, denial of faculties, and non-recognition of separatist sacramental claims, all deployed to enforce the continuity frame against competing readings. Theater ratio (0.30) is moderate: much of the interpretive labor (conciliar hermeneutics, doctrinal commissions) is genuine scholarly and pastoral work, but a growing share over the measured interval is defensive apologetic activity responding to challenges from both the rupture and composite readings rather than first-order theological development. accessibility_collapse (0.50) and resistance (0.60) are mid-range because unlike a mountain, alternative readings remain visibly and persistently live — the rupture and composite readings are not eliminated, only administratively disfavored, and resistance from separatist and progressive quarters has been sustained for six decades rather than dissipating.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial office and aligned faculties sit near the beneficiary end: they set the interpretive terms and their institutional standing depends on continuity holding. Traditionalist-leaning bishops are genuinely dual-positioned — beneficiaries of the discipline continuity affords against progressive overreach, but also payers in the sense that continuity forecloses their own preferred option of simply rejecting conciliar innovations as non-binding. Progressive reformers and vernacular advocates are clear targets: their preferred readings are declared unauthorized, and their institutional position inside the Church (rather than outside it) limits their exit options to 'constrained' rather than 'mobile.' Traditionalist separatist communities are the most severely targeted: 'trapped' exit reflects that leaving fully means losing recognized sacramental standing, and the continuity reading specifically denies the factual premise (rupture occurred) that would validate their separation as a defensible judgment rather than mere disobedience.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: this reading DOES solve a genuine coordination problem — six decades of governing a global institution through admittedly ambiguous conciliar texts requires SOME stable interpretive key, and continuity is not merely invented but tracks real textual and doctrinal continuities (e.g. the Council's own claim, in its documents, to stand in continuity with prior councils). The extraction is real but coexists with genuine coordination function, which is exactly the tangled_rope signature — distinguishing this from a pure snare where the coordination story would be mere cover. Declaring the founding problem as 'contested' rather than 'dead' honors that the ambiguity this reading was built to resolve has not, in fact, been resolved to universal satisfaction — the mandate persists because the underlying interpretive problem persists, not merely because of institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafting_intent_vs_textual_continuity,
    'Do the conciliar periti''s private drafting records (correspondence, diaries, committee minutes) support the continuity reading''s claim that ambiguous compromise language was intended to preserve prior doctrine, or do they reveal that key drafters intended and expected substantive doctrinal change that the final text''s ambiguity merely obscured?',
    'Systematic archival review of periti papers (many now available, e.g. Congar''s diaries, drafting committee records held at various institutes) cross-referenced against the final conciliar texts, conducted by historians without institutional stake in either reading''s vindication.',
    'If drafting intent consistently favored substantive change, the continuity reading''s textual argument weakens considerably and its extractive component (foreclosing the rupture reading) looks less like accurate interpretation and more like retrospective doctrinal management. If drafting intent is itself divided or ambiguous, this would support the composite-overdetermination reading over either continuity or rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_intent_vs_textual_continuity, empirical, 'Whether archival drafting evidence supports continuity''s account of authorial intent.').

omega_variable(
    thesis_hypothesis_reconciliation_validity,
    'Does the thesis/hypothesis distinction (or the broader doctrinal-development framework) actually reconcile Dignitatis Humanae''s religious liberty teaching with the Syllabus of Errors'' condemnation of religious indifferentism, or does it paper over a genuine doctrinal reversal by redescribing it as development?',
    'Close comparative textual and logical analysis of the specific propositions condemned in the Syllabus against the specific claims affirmed in Dignitatis Humanae, assessing whether they operate at the same level of magisterial commitment or whether the ''thesis/hypothesis'' framework is doing genuine logical work versus serving as an ad hoc harmonizing device.',
    'If the reconciliation genuinely holds, the continuity reading''s core doctrinal claim is vindicated on its strongest test case. If it does not hold, the continuity reading''s claimed absence of rupture is falsified at least for this doctrine, strengthening either the rupture or composite reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thesis_hypothesis_reconciliation_validity, conceptual, 'Whether the thesis/hypothesis reconciliation of DH and Syllabus is genuine or merely nominal.').

omega_variable(
    committer_framing_alternative,
    'Is the continuity/rupture/composite trichotomy itself the correct decomposition of the kernel, or does the continuity reading''s own insistence on a binary continuity-vs-rupture frame obscure that most actual historical actors (bishops, theologians, laity) held positions that do not map cleanly onto any single one of the three readings, varying document by document within the same conciliar corpus?',
    'Survey of contemporaneous theological literature and episcopal statements across the conciliar documents individually (rather than the Council as a whole) to determine whether continuity/rupture verdicts vary by document — e.g. Lumen Gentium read as continuous while Dignitatis Humanae is read as ruptural by the same authors.',
    'If document-level verdicts vary systematically, this favors treating each major conciliar document as its own kernel with its own reading-set, rather than treating ''Vatican II'' as a single kernel — which would itself be a further ε-invariance decomposition beyond the current three-reading split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the continuity/rupture/composite trichotomy is the right granularity for the kernel, or whether document-level decomposition is required.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.34).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1975, 0.42).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.46).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the vatican_ii_magisterial_authority kernel family. The rupture_reading authors a much higher ε (the interpretive discipline is read as actively suppressing recognition of genuine doctrinal reversal, with victims including the entire body of the faithful catechized under a false continuity premise). The composite_overdetermination_reading authors a structurally different beneficiary/victim map entirely (no single faction benefits from ambiguity-management; rather all factions are individually disadvantaged by the persistence of incompatible readings under one textual roof, pointing toward a piton-leaning classification for that sibling). All three stories must be read together to understand the kernel; none is a complete account alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
