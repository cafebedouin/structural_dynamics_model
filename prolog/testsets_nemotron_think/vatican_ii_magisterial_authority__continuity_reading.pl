% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Vatican II Continuity Reading — Organic Development in Unbroken Tradition
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The continuity reading of Vatican II presents itself as the organic
 *   development of the Church's unbroken tradition — a Mountain of
 *   hermeneutical nature. It claims the conciliar texts, properly read,
 *   constrain all implementation to preserve pre-conciliar doctrine: the
 *   'spirit of Vatican II' is unauthorized; SC §36's Latin mandate binds;
 *   Dignitatis Humanae's religious freedom reconciles with the Syllabus via
 *   the thesis/hypothesis distinction or development of doctrine.
 *   Operationally, this reading functions as an enforced interpretive regime:
 *   CDF notifications silence theologians; Traditionis Custodes restricts the
 *   1962 Missal; episcopal appointments favor continuity proponents;
 *   catechetical texts are vetted for hermeneutical compliance. The
 *   constraint extracts interpretive freedom from progressive theologians,
 *   rupture-reading proponents, and local churches, transferring authority to
 *   the Roman center. It claims natural emergence (organic development) but
 *   requires active enforcement (canonical penalties, liturgical legislation,
 *   appointment control). Beneficiaries are identifiable: traditionalist
 *   communities, CDF, conservative bishops. Victims are identifiable:
 *   silenced theologians, denied communities, excluded rupture/composite
 *   readers. This is a false summit Mountain candidate — FSM should trigger.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.72).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, mountain).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Continuity Reading — Organic Development in Unbroken Tradition").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).
domain_priors:emerges_naturally(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '29958bb1-7896-459e-af4c-c8078d4744ab').
narrative_ontology:cs_kernel_codification('29958bb1-7896-459e-af4c-c8078d4744ab', fixed_text).
narrative_ontology:cs_authority_grounding('29958bb1-7896-459e-af4c-c8078d4744ab', lineage).
narrative_ontology:cs_interpretation_layer_present('29958bb1-7896-459e-af4c-c8078d4744ab').
narrative_ontology:cs_reading_relation('29958bb1-7896-459e-af4c-c8078d4744ab', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('29958bb1-7896-459e-af4c-c8078d4744ab', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('29958bb1-7896-459e-af4c-c8078d4744ab', foundational, organic_development_preserves_substance).
narrative_ontology:cs_axiom_status(organic_development_preserves_substance, holdable).
narrative_ontology:cs_axiom_grounding('29958bb1-7896-459e-af4c-c8078d4744ab', organic_development_preserves_substance, deontological).
narrative_ontology:cs_axiom('29958bb1-7896-459e-af4c-c8078d4744ab', foundational, conciliar_texts_constrain_implementation).
narrative_ontology:cs_axiom_status(conciliar_texts_constrain_implementation, holdable).
narrative_ontology:cs_axiom_grounding('29958bb1-7896-459e-af4c-c8078d4744ab', conciliar_texts_constrain_implementation, conventional).
narrative_ontology:cs_axiom('29958bb1-7896-459e-af4c-c8078d4744ab', secondary, latin_preservation_mandate_binding).
narrative_ontology:cs_axiom_status(latin_preservation_mandate_binding, holdable).
narrative_ontology:cs_axiom_grounding('29958bb1-7896-459e-af4c-c8078d4744ab', latin_preservation_mandate_binding, conventional).
narrative_ontology:cs_axiom('29958bb1-7896-459e-af4c-c8078d4744ab', secondary, religious_freedom_reconcilable_via_thesis_hypothesis).
narrative_ontology:cs_axiom_status(religious_freedom_reconcilable_via_thesis_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('29958bb1-7896-459e-af4c-c8078d4744ab', religious_freedom_reconcilable_via_thesis_hypothesis, instrumental).
narrative_ontology:cs_reference_frame('29958bb1-7896-459e-af4c-c8078d4744ab', organic_development_continuity).
narrative_ontology:cs_drift_state('29958bb1-7896-459e-af4c-c8078d4744ab', contemporary_traditionalist_resurgence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('29958bb1-7896-459e-af4c-c8078d4744ab', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_catholics).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, cd_f_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, conservative_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditional_liturgy_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_proponents).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, communities_denied_older_forms).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, religious_freedom_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, development_of_doctrine_thesis_hypothesis).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, latin_liturgical_primacy).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, syllabus_compatibility_of_dh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Identify their Catholic identity with the continuity reading; the constraint validates their self-understanding and liturgical preferences. Exit would require restructuring identity, not merely changing parish. They benefit from institutional recognition of their form of worship and doctrinal emphasis.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_catholics, beneficiary,
    organized, generational, identity_locked, global).

% The Congregation for the Doctrine of the Faith (now Dicastery) enforces the continuity reading through notifications, recognitio, and canonical penalties. It benefits institutionally from being the authoritative interpreter. Its exit options are analytical — it could change course but only through magisterial act, not individual choice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, cd_f_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, cd_f_authority, beneficiary).

% Implement the continuity reading in dioceses: liturgical norms, catechetical oversight, seminary formation. They benefit from the clarity and authority the reading provides. Constrained exit — they could resist but face canonical and career consequences; some do resist quietly.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, conservative_bishops, agenda_setter,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, conservative_bishops, beneficiary).

% Communities attached to the 1962 Missal and pre-conciliar disciplines. The continuity reading (especially under Summorum Pontificum / Traditionis Custodes dynamics) determines their canonical status. Identity-locked: their communal identity is constituted by this form; exit means dissolution of the community.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditional_liturgy_communities, beneficiary,
    moderate, biographical, identity_locked, local).

% Theologians whose work engages rupture or composite readings face CDF notifications, withdrawal of missio canonica, silencing, or marginalization. They bear the cost of the constraint's enforcement. Constrained exit: could leave academic theology or the Church, but at high professional and vocational cost.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Those who read Vatican II as a fundamental break — including many European theologians, some synodal voices. They are excluded from authoritative magisterial channels; their reading is treated as non-reception. They pay by being structurally barred from shaping official teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_proponents, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_proponents, payer).

% Lay faithful and priests who desire the older liturgical form but are denied it by bishops implementing Traditionis Custodes under the continuity reading's hermeneutic. Trapped: no canonical avenue, geographic mobility limited, identity tied to parish.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, communities_denied_older_forms, payer,
    powerless, biographical, trapped, local).

% Scholars and jurists who read Dignitatis Humanae as a genuine doctrinal development incompatible with the Syllabus's condemnations. The continuity reading's thesis/hypothesis distinction constrains their interpretation. Mobile: can work in secular academia or other traditions, but lose Catholic institutional voice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, religious_freedom_advocates, payer,
    moderate, generational, mobile, global).

% Experience the constraint through liturgical availability, catechetical content, and pastoral tone. Some benefit from clarity and stability; others pay through alienation when their lived experience or cultural context clashes with the enforced hermeneutic. Constrained exit: parish-shopping limited, leaving the Church is high-cost.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, ordinary_faithful, beneficiary).

% The pope and curia hold ultimate authority to adjudicate the kernel. Francis's Traditionis Custodes and Benedict's Summorum Pontificum represent opposing papal acts on the same kernel. Analytical exit: the office itself cannot exit, only the occupant's interpretive act changes.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, magisterium_papacy, agenda_setter,
    institutional, civilizational, analytical, universal).

% Scholars (e.g., Ratzinger's later critiques, O'Malley, Faggioli) who read the texts as ambiguous compromises encoding incompatible visions. Excluded from magisterial authority structures; mobile in academic terms but excluded from canonical teaching roles.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, composite_overdetermination_proponents, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutical key for reading sixteen conciliar documents across sixty years: prevents fragmentation of magisterial teaching by requiring all post-conciliar implementation to demonstrate continuity with the pre-conciliar deposit of faith. Solves the coordination problem of 'what does the Council mean?' for a global communion.
% TRANSFER_FUNCTION: Moves interpretive authority from local bishops, theologians, and pastoral actors to the Roman center (CDF, papacy). Moves liturgical and catechetical resources toward the 1962 form and continuity-aligned materials. Moves career advancement and canonical standing toward those who publicly embody the continuity reading; away from those who do not.
% ABSENT_VOICES: The majority of post-conciliar Latin American, African, and Asian episcopates who received the Council through inculturation and liberation hermeneutics — not the European continuity/rupture binary. Also absent: the lived experience of lay movements (Focolare, Communion and Liberation, Sant'Egidio) whose charismatic readings precede hermeneutical categories. They are not in the Roman curial conversation that enforces this reading.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the hermeneutical framework governing CDF notifications, liturgical law (Traditionis Custodes), episcopal appointments, seminary curricula, and catechetical texts would collapse. Bishops would regain local interpretive authority; theologians would publish without Roman pre-censorship; the 1962 Missal's canonical status would revert to diocesan discretion. The global communion's doctrinal unity mechanism would shift from Roman hermeneutical enforcement to synodal/reception-based processes.
% FOUNDING_PROBLEM: Post-conciliar chaos (1965-1980s): wild liturgical experimentation, catechetical collapse, doctrinal confusion, mass exodus of clergy and religious, contradictory episcopal conferences. The continuity reading was constructed as the hermeneutical brake: a single authoritative key to stop the centrifugal forces threatening communion.
% FOUNDING_PROBLEM_CORROBORATION: The continuity reading's proponents (Ratzinger/Benedict XVI, CDF, traditionalist bishops) attest the founding problem remains live — citing ongoing liturgical abuse, doctrinal confusion, and synodal fragmentation. Critics (majority of theologians, many episcopal conferences, Francis's synodal emphasis) attest the founding problem is substantially solved or misdiagnosed: the chaos was transitional, and the continuity reading now functions as ideological control. No neutral corroboration exists; the dispute IS the kernel's reception history.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vatican_ii_magisterial_authority__continuity_reading),
    narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high: the constraint extracts interpretive autonomy, liturgical choice, and theological labor from a global communion, concentrating authority in Rome. Suppression (0.78) is higher: persistence depends on actively excluding alternative readings (rupture, composite) through canonical machinery, not on persuasive force. Theater (0.48) is near the pivot: genuine coordination (hermeneutical unity for 1.4B Catholics) coexists with performative enforcement (investigating theologians for footnotes, restricting Masses that harm no one). Accessibility collapse (0.68) is high but not total — rupture and composite readings persist in academia and local practice despite suppression. Resistance (0.73) is high: theologians publish abroad, bishops implement TC variably, lay movements ignore the binary. The measurement series shows extraction and suppression rising over 60 years as the enforcement apparatus matured; theater rises as coordination function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   From the CDF/traditionalist seat, the constraint IS the Mountain — organic development is the nature of Tradition; enforcement merely protects the deposit. From the progressive theologian seat, it is a Snare — the 'hermeneutic of continuity' is a rhetorical cover for suppressing Vatican II's actual opening to modernity. From the ordinary faithful seat, it oscillates: a Rope when it provides stable catechesis, a Snare when it denies pastoral reality (divorced/remarried, LGBT, women's roles). The engine computes this divergence from the structural data; the authored claim (Mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   CDF and papacy are structural beneficiaries (d ~ 0.1): they collect interpretive authority and canonical control. Traditionalist Catholics and traditional_liturgy_communities are beneficiaries (d ~ 0.2): they receive canonical recognition and resource allocation. Conservative bishops are agenda_setters with beneficiary capture (d ~ 0.25). Progressive theologians, rupture proponents, and denied communities are payers (d ~ 0.85-0.95): they bear silencing, restriction, exclusion. Ordinary faithful are dual (d ~ 0.5): some benefit from stability, others pay through alienation. Religious freedom advocates are mobile payers (d ~ 0.6): they can exit to secular academia but lose Catholic voice. Composite proponents are excluded mobile (d ~ 0.4): excluded from authority but mobile in scholarship. The identity_locked exit of traditionalist communities and denied communities creates asymmetric trapping: both cannot leave without identity dissolution, but only one holds the enforcement lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-conciliar chaos) was real and live in 1980. By 2000, the chaos had largely resolved through reception — but the continuity reading's enforcement apparatus (CDF, liturgical law, appointment criteria) had institutionalized and acquired self-preservation incentives. The mandate (hermeneutical unity) atrophied into a mechanism for ideological conformity. The constraint now persists because the administrator (CDF/papacy) could change it but the cost to fix (admitting the reading was always contested, restructuring canonical enforcement) exceeds what the administrator bears. This is mandatrophy: the arrangement solves yesterday's problem with today's extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_naturalness_ambiguity,
    'Is the ''hermeneutic of continuity'' a genuine philosophical-theological principle discoverable in the texts, or an ideological lens imposed from outside to constrain reception?',
    'Comparative textual analysis: do the conciliar documents, read on their own terms and in their drafting history (acta), support a single coherent continuity reading? Or do they require the continuity lens to produce continuity? Historical reception study: how did the Council Fathers themselves understand the texts in 1965-1970?',
    'If the hermeneutic is textually grounded, the Mountain claim has merit and extraction is the cost of preserving truth. If imposed, the Mountain claim is a false summit and the constraint is Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_naturalness_ambiguity, conceptual, 'Whether the continuity reading''s self-presentation as natural law of tradition is textually warranted.').

omega_variable(
    thesis_hypothesis_viability,
    'Can the thesis/hypothesis distinction (or development of doctrine) genuinely reconcile Dignitatis Humanae with the Syllabus of Errors, or is it a post-hoc rationalization that collapses under scrutiny?',
    'Systematic theological engagement: does the distinction preserve the Syllabus''s intended condemnations while accommodating DH''s positive doctrine? Or does it effectively reverse the Syllabus while preserving its vocabulary? Magisterial reception: has any post-conciliar magisterial act formally endorsed this specific reconciliation?',
    'If viable, the continuity reading''s claim to doctrinal coherence is strengthened (lower extractiveness). If not, the reconciliation is extraction — the constraint forces a false harmony to maintain the Mountain claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thesis_hypothesis_viability, conceptual, 'Whether the continuity reading''s key doctrinal reconciliation is coherent or performative.').

omega_variable(
    latin_mandate_function,
    'Does SC §36''s Latin preservation mandate function as genuine liturgical coordination (universal language, unity) or as an exclusionary barrier that suppresses vernacular inculturation and the 1962 Missal''s competitors?',
    'Sociological study: in communities where Latin is preserved, does it function as unifying coordination or as identity boundary? Comparative: Eastern Catholic churches maintain ancient liturgical languages without suppression of vernacular — is the Latin mandate structurally different?',
    'If coordination, the mandate is Rope-like (low extraction). If exclusionary, it is Snare-like (high extraction, suppression of inculturation). The continuity reading''s enforcement of Latin via Traditionis Custodes suggests the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_mandate_function, empirical, 'Whether the Latin preservation mandate coordinates or excludes.').

omega_variable(
    cs_framing_underdetermination,
    'Does the continuity reading''s authority ground in ''lineage'' (living Tradition) or ''extraction'' (institutional self-preservation)? The same structural data supports both framings.',
    'Trace the personnel and institutional interests: do CDF interventions correlate with doctrinal threats or with challenges to curial authority? Compare Benedict XVI''s theological continuity project with Francis''s synodal continuity project — same kernel, different authority grounding.',
    'If lineage, the CS structure is legitimate authority with interpretive layer. If extraction, the CS structure is a self-enforcing ideology. Changes authority_grounding in cs_structure and triggers different Boltzmann coupling thresholds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the commitment system''s authority grounding is lineage or extraction — the same structural facts support rival framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican2_cont_tr_t0, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vatican2_cont_tr_t10, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vatican2_cont_tr_t20, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(vatican2_cont_tr_t30, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(vatican2_cont_tr_t40, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(vatican2_cont_tr_t50, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement(vatican2_cont_tr_t60, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(vatican2_cont_be_t0, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vatican2_cont_be_t10, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(vatican2_cont_be_t20, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vatican2_cont_be_t30, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(vatican2_cont_be_t40, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(vatican2_cont_be_t50, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(vatican2_cont_be_t60, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vatican2_cont_su_t0, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vatican2_cont_su_t10, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(vatican2_cont_su_t20, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(vatican2_cont_su_t30, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(vatican2_cont_su_t40, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(vatican2_cont_su_t50, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(vatican2_cont_su_t60, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, tridentine_mass_canonical_status).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, cdf_doctrinal_enforcement_regime).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, synodal_church_reception_process).

% DUAL FORMULATION NOTE:
% This constraint is the continuity_reading of the vatican_ii_magisterial_authority kernel family. It claims Mountain (organic development = natural law of tradition) but operates with active enforcement, identifiable beneficiaries/victims, and rising extraction — a false summit Mountain candidate. The rupture_reading claims the kernel is a break (different ε, different victims). The composite_overdetermination_reading claims the kernel's texts are structurally ambiguous (different coordination function). All three share the same referent (the sixteen conciliar documents) but instantiate different constraints with different ε, stakeholders, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, organized, 0.22).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
