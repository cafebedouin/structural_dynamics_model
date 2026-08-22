% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority (Rupture Reading): Council Defective, Modernist Capture
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   This constraint instantiates the rupture reading of Vatican II: the
 *   Council's documents contain substantive doctrinal discontinuities with
 *   pre-conciliar teaching, and the post-conciliar Church has been captured
 *   by a reformist faction whose interests lie in consolidating those changes
 *   while maintaining a cover narrative of organic development. The
 *   traditional Catholic community experiences this as doctrinal
 *   destabilization and abandonment of inherited practice, but is suppressed
 *   from voicing that experience — Rome insists continuity through
 *   institutional authority. The constraint operates as a tangled_rope:
 *   genuine coordination function (maintaining institutional unity under
 *   reformed leadership) coupled with asymmetric extraction (traditionalists
 *   bear the cost of doctrinal disorientation and identity loss; reformists
 *   collect the benefit of authority to reshape the Church). The suppression
 *   is both structural (institutional sanctions against traditionalist
 *   scholarship, exclusion of SSPX) and performative (theater through
 *   repeated institutional reassertions of continuity).
 *
 * KEY AGENTS:
 *   - traditional_catholic_community: primary victims, identity-locked, bearing the cost of cognitive dissonance between lived rupture and mandated continuity narrative
 *   - post_conciliar_reformist_faction: primary beneficiaries, arbitrage-capable (can move between different interpretations as institutional conditions allow), consolidating their theological and pastoral authority
 *   - vatican_bureaucratic_apparatus: agenda-setter, administers the continuity narrative, enforces suppression of rupture-reading scholarship and traditionalist dissent
 *   - sspx_schismatic_body: excluded alternative, instantiates the structural possibility that would erupt if the constraint failed — their existence and condemnation depend on Rome's continuity assertion
 *   - doctrinal_magisterium_defenders: trapped between intellectual honesty about doctrinal shifts and institutional pressure to affirm continuity, constrained by career/standing costs of rupture-reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority (Rupture Reading): Council Defective, Modernist Capture").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theological/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '019a2142-b142-46a8-94f5-9143d4d672ec').
narrative_ontology:cs_kernel_codification('019a2142-b142-46a8-94f5-9143d4d672ec', fixed_text).
narrative_ontology:cs_authority_grounding('019a2142-b142-46a8-94f5-9143d4d672ec', extraction).
narrative_ontology:cs_interpretation_layer_present('019a2142-b142-46a8-94f5-9143d4d672ec').
narrative_ontology:cs_reading_relation('019a2142-b142-46a8-94f5-9143d4d672ec', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('019a2142-b142-46a8-94f5-9143d4d672ec', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('019a2142-b142-46a8-94f5-9143d4d672ec', foundational, vatican_ii_documents_contain_doctrinal_rupture).
narrative_ontology:cs_axiom_status(vatican_ii_documents_contain_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('019a2142-b142-46a8-94f5-9143d4d672ec', vatican_ii_documents_contain_doctrinal_rupture, empirically_contingent).
narrative_ontology:cs_axiom('019a2142-b142-46a8-94f5-9143d4d672ec', foundational, magisterial_continuity_principle_violated).
narrative_ontology:cs_axiom_status(magisterial_continuity_principle_violated, holdable).
narrative_ontology:cs_axiom_grounding('019a2142-b142-46a8-94f5-9143d4d672ec', magisterial_continuity_principle_violated, deontological).
narrative_ontology:cs_axiom('019a2142-b142-46a8-94f5-9143d4d672ec', secondary, post_conciliar_magisterium_captures_council_for_modernism).
narrative_ontology:cs_axiom_status(post_conciliar_magisterium_captures_council_for_modernism, holdable).
narrative_ontology:cs_axiom_grounding('019a2142-b142-46a8-94f5-9143d4d672ec', post_conciliar_magisterium_captures_council_for_modernism, empirically_contingent).
narrative_ontology:cs_reference_frame('019a2142-b142-46a8-94f5-9143d4d672ec', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('019a2142-b142-46a8-94f5-9143d4d672ec', contemporary_post_2010, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('019a2142-b142-46a8-94f5-9143d4d672ec', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_reformist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, vatican_bureaucratic_apparatus).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_community).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_certainty_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_magisterium_defenders).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, magisterial_continuity_doctrine_violated).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, apostolic_tradition_rupture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the cost of doctrinal instability and perceived abandonment of pre-conciliar practices and teaching. Exit means leaving the Church entirely or joining schismatic bodies (SSPX), severing a constitutive identity and severing communion with inherited tradition. The constraint operates through authoritative denial that rupture occurred: Rome insists continuity, while lived experience registers radical discontinuity. This denial (the suppression) is itself the mechanism through which the cost is imposed.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_community, payer,
    moderate, generational, identity_locked, global).

% Gains institutional authority and legitimacy to reshape Catholic practice, theology, and pastoral approach. The constraint frames their innovations as 'development' or 'reform' rather than as rupture, protecting them from charges of doctrinal error and giving them the mantle of legitimate authority. They benefit from the ambiguity: they can claim continuity when defending against traditionalist critique, while advancing substantially novel positions.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_reformist_faction, beneficiary,
    organized, biographical, arbitrage, global).

% Sets and enforces the official narrative (continuity through hermeneutic of reform); administers the constraint by controlling magisterial interpretation, managing dissent, and reframing rupture claims as misreadings. The apparatus benefits from the constraint's ambiguity: it can consolidate power by appearing to maintain tradition while actually accelerating institutional transformation. Cannot openly admit rupture without delegitimizing itself.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_bureaucratic_apparatus, agenda_setter,
    institutional, generational, trapped, global).

% Represents the structural alternative to submission under the constraint: they read Vatican II as rupture and refuse the post-conciliar magisterium's authority. Their exclusion from communion is enforced by Rome's insistence on continuity narrative; if rupture were acknowledged, the SSPX's foundational critique would require institutional reckoning rather than dismissal as schismatic error.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_schismatic_body, excluded,
    moderate, generational, constrained, regional).

% Academic theologians, bishops, and clergy committed to pre-conciliar doctrine who must navigate the constraint by either accepting the continuity narrative (intellectually costly) or being marginalized as disloyal or ignorant. Exit means professional marginalization, loss of academic or institutional standing, or submission to the reformist consensus.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_magisterium_defenders, payer,
    organized, generational, constrained, global).

% The fixed texts themselves — analyzed here as a non-agent entity, a boundary object around which the constraint operates. The documents are subject to competing hermeneutics: continuity reading interprets them as developing doctrine; rupture reading identifies internal contradictions and novel propositions irreconcilable with prior teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, vatican_council_ii_documents, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, vatican_council_ii_documents).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, post_conciliar_reformist_faction).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the post-conciliar Church around a single authoritative interpretation of Vatican II as organic development, ensuring institutional unity under reformed leadership by preventing schism and marginalizing traditionalist objections.
% TRANSFER_FUNCTION: Transfers doctrinal authority from the traditional deposit (magisterial continuity doctrine) to the post-conciliar interpreters; transfers institutional legitimacy and pastoral control to reformist faction; transfers identity-cost (doctrinal disorientation, loss of familiar liturgy and theology) to traditionalist communities.
% ABSENT_VOICES: Schismatic traditionalists (SSPX) are structurally excluded and delegitimized; pre-conciliar theologians are marginalized or reinterpreted; lay Catholics who experience rupture as lived discontinuity are told they misunderstand; opposing theological schools are absorbed into the continuity narrative rather than engaged as intellectual equals.
% DISAPPEARANCE_RATIONALE: If the constraint (the enforced narrative of continuity and authority behind Vatican II) vanished overnight, the Church would face an immediate crisis of legitimacy: traditionalists would seek formal restoration of pre-conciliar practices and doctrine; the reformist apparatus would lose the shield that protects its innovations as 'development'; schismatic bodies would be reintegrated or openly challenged rather than marginalized; doctrinal investigation would be reopened; the institutional coherence held together by narrative assertion would collapse into competing jurisdictional and theological claims.
% FOUNDING_PROBLEM: Vatican II was convened to address perceived gaps in the Church's engagement with modernity, to clarify doctrine on religious liberty and the Church's relationship to secular institutions, and to renew liturgical and pastoral practice. The founding problem is stated as the need for aggiornamento (updating) while preserving continuity with tradition.
% FOUNDING_PROBLEM_CORROBORATION: The Council itself and Paul VI attest the founding problem remains live (ongoing modernization needs). SSPX and traditionalist theologians attest the founding problem was invoked as cover for a rupture that violated the Church's own principle of magisterial continuity; independent historians (Danièlou, de Mattei, Chadwick) document that the Council's actual outcomes diverged substantially from its stated continuity intention. The rupture is corroborated from outside the reformist beneficiary faction.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness accumulates over the interval (0.15 → 0.68) as post-conciliar innovations crystallize and traditionalist objections are definitively ruled out of bounds. Theater rises sharply (0.25 → 0.58) as institutional theater around 'authentic conciliar interpretation' and 'hermeneutic of continuity' becomes necessary to hold the continuity line against accumulating evidence of doctrinal shift. Suppression requirement rises (0.35 → 0.72) as traditionalist resistance hardens: SSPX consolidates, traditionalist scholarships develop coherent critiques, and Rome must invest more enforcement energy in shutting down rupture-reading scholarship and marginalizing traditionalist movements. The measurements are authored on one shared time grid (1962, 1972, 1982, 1992, 2002, 2010), matching every metric at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the Vatican apparatus and reformist-faction seats, the constraint appears as genuine coordination: Vatican II was a legitimate Council, its interpretation is a live matter of theological development, and institutional unity under reformed leadership is a real benefit requiring active defense against schismatic fragmentation. From the traditional community and doctrinal-defender seats, the same structure appears as enforced extraction under false continuity cover: the documents show rupture, Rome knows this, Rome suppresses the acknowledgment through institutional authority, and traditionalists bear the costs of being told they are wrong to perceive the discontinuity they actually experience. The engine will compute these divergences from the directionality data — this is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The Vatican bureaucratic apparatus (institutional power, trapped by the constraint itself — cannot admit rupture without destroying its own authority) sits as beneficiary-near-symmetric: it benefits from the constraint but is also bound by it. The post-conciliar reformist faction (organized power, arbitrage-capable — can adapt their positions as institutional winds shift) sits as beneficiary-target: they collect authority and legitimacy while the constraint remains their shield against traditionalist challenge. The traditional Catholic community (moderate power, identity-locked — exit means severing constitutive identity with the Church) sits as target-full: they bear the extraction (doctrinal disorientation, loss of familiar practices) while suppression prevents them from naming that extraction. The doctrinal magisterium defenders (organized power, constrained exit through professional stakes) sit as target-partial: they are trapped between intellectual perception of rupture and institutional pressure to affirm continuity, bearing the cost of cognitive dissonance without collecting the benefits of reformist authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for aggiornamento and modernization engagement) has a contested status: reformists attest it is live (the Church must continue updating), traditionalists attest it has been invoked as cover for rupture and that genuine continuity would better serve the Church. The disappearance verdict is world_rearranges: if the constraint vanished (if Rome acknowledged rupture), the Church would immediately face institutional crisis, schismatic reintegration demands, and doctrinal reckoning. This gap (founding problem = contested status, disappearance verdict = world_rearranges) indicates the constraint operates as cover for an institutional transformation that cannot admit its own nature. The theater_ratio accumulation (0.25 → 0.58) and suppression_requirement rise (0.35 → 0.72) are consistent with a constraint whose original coordination function (addressing modernization) has been replaced by a purely extractive and theatrical function (defending institutional authority and reformist prerogative against traditionalist challenge). Mandatrophy is incipient: the founding problem is no longer what drives the constraint; the constraint now drives itself through institutional theater and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_kernel_ambiguity,
    'Is Vatican II a single kernel with multiple defensible readings, or are the rupture and continuity theses incommensurable claims about fundamentally different things (different kernels)?',
    'Textual analysis: can a single coherent framework account for the Council''s actual language AND its traditional precedent? Or does the Council itself contain irreconcilable formulations that force a choice between readings rather than a pluralistic interpretation?',
    'If a single kernel accommodates both readings, this is a composite_overdetermination case (a third sibling reading). If the readings are incommensurable, one kernel cannot hold both — only the rupture reading''s core premise is consistent with the evidence of doctrinal shift and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_ambiguity, empirical, 'Whether Vatican II is one kernel or a conceptual boundary dispute.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (denial of rupture, enforcement of continuity narrative) structural (institutional sanctions, magisterial authority blocking alternative readings) or internalized (bishops and theologians have genuinely internalized the continuity framing)?',
    'Post-exit trajectory analysis: when traditionalists leave (SSPX, sede vacantists) or when reformist pressure eases (during Benedict XVI), do they maintain suppression beliefs or revert? Private documents (bishops'' diaries, Vatican archives post-2030) revealing private doubts about continuity while public statements maintain it would indicate internalization masking suppression awareness.',
    'If suppression is purely structural, it is the mechanism of the constraint and can be unraveled by changing institutional rules. If internalized, the constraint travels with those who exit and cannot be fixed by formal policy change alone — the identity lock is deeper. This affects whether the constraint is truly tangled_rope (structural coordination + enforcement) or more snare-like (structural extraction + internalized suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Suppression mechanism: institutional coercion vs. absorbed legitimacy claim.').

omega_variable(
    kernel_fracture_or_hermeneutic_instability,
    'Is the inability of post-conciliar interpretation to settle on a stable reading of Vatican II evidence of hermeneutic failure (the continuity framing cannot coherently contain the documents'' actual content), or evidence of legitimate theological pluralism under a unified magisterium?',
    'Comparison with prior Councils: did Trent, Vatican I, or other Councils exhibit comparable hermeneutic drift? If Vatican II is an outlier in the degree of reinterpretation required to maintain the continuity claim, that suggests the documents themselves contain propositions that resist continuity reading. If prior Councils also required substantial reinterpretation, hermeneutic drift may be normal conciliar life, not evidence of rupture.',
    'If Vatican II is an outlier, the rupture reading gains empirical support — the constraint''s need for suppression and theater becomes explicable (the continuity claim must be defended through authority assertion, not through coherent textual argument). If not an outlier, the failure of interpretation may reflect broader patterns in how doctrinal authority operates, and the rupture/continuity distinction becomes less sharp.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_fracture_or_hermeneutic_instability, empirical, 'Is Vatican II hermeneutically exceptional, or is the doctrinal instability it exhibits a recurring feature of conciliar reception?').

omega_variable(
    beneficiary_capture_vs_genuine_reform,
    'Did the post-conciliar reformist faction capture Vatican II to advance agendas incompatible with the Council''s actual mandate, or did the Council itself create institutional conditions enabling that capture?',
    'Historical reconstruction: compare the Council''s closing documents (what was formally decided) against the direction of post-conciliar implementation. Where they diverge, examine the deliberative record (periti papers, voting records, editorial decisions) to determine whether reformists exploited ambiguity in the Council''s language or whether the Council itself endorsed the implementations that followed.',
    'If capture occurred (reformists twisted a genuinely reform-continuous Council toward rupture), the constraint becomes snare-like and the victims are those deceived by the Council''s stated intent. If the Council itself opened the door (through ambiguous language, poorly vetted propositions, or reformist-majority intentions), the constraint remains tangled_rope but the founding problem corroboration shifts: the Council''s own documents authorize the rupture, not just reformist misreading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_reform, empirical, 'Did reformists capture Vatican II, or did it enable the transformations that followed?').

omega_variable(
    doctrine_vs_discipline_boundary,
    'Are the post-conciliar changes (vernacular liturgy, altar orientation, married permanent deacons, ecumenical openness) doctrinal ruptures or disciplinary reforms within an unchanging deposit of faith?',
    'Semantic analysis: do the Council documents themselves distinguish doctrine (universal, unchangeable teaching on faith and morals) from discipline (changeable practice)? Where the distinction is explicit, examine whether post-conciliar innovations respect it. Where the Council blurs the boundary (e.g., Unitatis Redintegratio on ecumenism mixes doctrinal claims about other Christian communities with disciplinary openness to dialogue), determine whether the blurring was intentional or the site of hermeneutic capture.',
    'If the changes are purely disciplinary, the continuity reading is defensible even if practice radically shifts. If the changes entail doctrinal reframings (the nature of tradition, the role of Scripture, the relationship between natural law and pastoral prudence), the rupture claim gains support — the suppression becomes necessary to hold the continuity line against textual evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_discipline_boundary, empirical, 'Are post-conciliar changes doctrinal or disciplinary?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1972, vatican_ii_authority__rupture_reading, theater_ratio, 1972, 0.35).
narrative_ontology:measurement_basis(vati_tr_t1972, observed).
narrative_ontology:measurement(vati_tr_t1982, vatican_ii_authority__rupture_reading, theater_ratio, 1982, 0.48).
narrative_ontology:measurement_basis(vati_tr_t1982, observed).
narrative_ontology:measurement(vati_tr_t1992, vatican_ii_authority__rupture_reading, theater_ratio, 1992, 0.54).
narrative_ontology:measurement_basis(vati_tr_t1992, observed).
narrative_ontology:measurement(vati_tr_t2002, vatican_ii_authority__rupture_reading, theater_ratio, 2002, 0.57).
narrative_ontology:measurement_basis(vati_tr_t2002, observed).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_authority__rupture_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2010, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1972, vatican_ii_authority__rupture_reading, base_extractiveness, 1972, 0.42).
narrative_ontology:measurement_basis(vati_be_t1972, observed).
narrative_ontology:measurement(vati_be_t1982, vatican_ii_authority__rupture_reading, base_extractiveness, 1982, 0.58).
narrative_ontology:measurement_basis(vati_be_t1982, observed).
narrative_ontology:measurement(vati_be_t1992, vatican_ii_authority__rupture_reading, base_extractiveness, 1992, 0.65).
narrative_ontology:measurement_basis(vati_be_t1992, observed).
narrative_ontology:measurement(vati_be_t2002, vatican_ii_authority__rupture_reading, base_extractiveness, 2002, 0.67).
narrative_ontology:measurement_basis(vati_be_t2002, observed).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_authority__rupture_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement_basis(vati_be_t2010, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.35).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1972, vatican_ii_authority__rupture_reading, suppression_requirement, 1972, 0.54).
narrative_ontology:measurement_basis(vati_su_t1972, observed).
narrative_ontology:measurement(vati_su_t1982, vatican_ii_authority__rupture_reading, suppression_requirement, 1982, 0.66).
narrative_ontology:measurement_basis(vati_su_t1982, observed).
narrative_ontology:measurement(vati_su_t1992, vatican_ii_authority__rupture_reading, suppression_requirement, 1992, 0.71).
narrative_ontology:measurement_basis(vati_su_t1992, observed).
narrative_ontology:measurement(vati_su_t2002, vatican_ii_authority__rupture_reading, suppression_requirement, 2002, 0.72).
narrative_ontology:measurement_basis(vati_su_t2002, observed).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_authority__rupture_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement_basis(vati_su_t2010, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, catholic_traditional_identity__post_conciliar_crisis).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, magisterial_authority__doctrinal_instability).

% DUAL FORMULATION NOTE:
% Vatican II authority is a contested kernel with three reading-based constraint instantiations. This story (rupture_reading) instantiates the interpretation that the Council represents doctrinal rupture and that post-conciliar Church is under modernist capture. The continuity_reading constraint instantiates Rome's official position that Vatican II represents organic development. The composite_overdetermination_reading constraint instantiates a third position: that the Council is an overdetermined composite incapable of coherent unification. All three share the same kernel (Vatican II documents and authority) but differ fundamentally in their ε values, beneficiary/victim structures, and claimed types. Sibling constraints linked via network.affects_constraints model the structural entanglement: changes in one reading's institutional status (e.g., if Rome acknowledged rupture, shifting to the rupture reading) would immediately affect the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
