% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Suppression of Plural Marriage (Exogenous Override Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) marked the LDS church's
 *   public abandonment of plural marriage doctrine. Under this reading
 *   (exogenous_override), the Manifesto is not a legitimate doctrinal
 *   reinterpretation but a document of capitulation: the federal government's
 *   coercive apparatus (imprisonment, property seizure, disenfranchisement,
 *   territorial non-admission) forced the institutional church to reframe a
 *   core theological claim (D&C 132, 'eternal plural marriage') as no longer
 *   divinely mandated. The constraint is the federal suppression regime,
 *   operating through prosecution and property seizure. The theater is high
 *   because the Manifesto cloaks coercive capitulation in the language of
 *   prophetic revelation, creating an illusion of voluntary institutional
 *   reinterpretation. Practicing polygamists become victims not only of
 *   federal enforcement but of institutional betrayal — their church
 *   redefines the very doctrine they understood as divine. The theater ratio
 *   rises sharply after 1890 because the suppression machinery shifts from
 *   explicit coercion (arrests, imprisonments) to institutional
 *   self-enforcement (the church policing its own members under the cover of
 *   the Manifesto).
 *
 * KEY AGENTS:
 *   - Federal government: institutional agenda-setter, primary beneficiary (achieves territorial religious uniformity and confiscates LDS property)
 *   - LDS institutional leadership: dual-positioned payer-beneficiary (survives institutionally by capitulating to federal authority; loses doctrinal integrity and moral authority with strict-observance members)
 *   - Latter-day Saint polygamists: powerless, identity-locked payers (cannot exit without doctrinal self-renunciation; face imprisonment and family dissolution)
 *   - Polygamist families: trapped victims (household dissolution, economic collapse, loss of legal protections)
 *   - Federal courts: institutional agenda-setter machinery (legitimate suppression through constitutional interpretation)
 *   - Protestant cultural establishment: institutional beneficiary (monogamy-only norm established as cultural orthodoxy)
 *   - Strict-observance dissident polygamists: excluded voice (would testify the 1890 Manifesto is capitulation, not revelation; face same federal prosecution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.89).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Suppression of Plural Marriage (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '84f26535-5fa3-489e-8316-da487d78fc26').
narrative_ontology:cs_kernel_codification('84f26535-5fa3-489e-8316-da487d78fc26', fixed_text).
narrative_ontology:cs_authority_grounding('84f26535-5fa3-489e-8316-da487d78fc26', extraction).
narrative_ontology:cs_interpretation_layer_present('84f26535-5fa3-489e-8316-da487d78fc26').
narrative_ontology:cs_reading_relation('84f26535-5fa3-489e-8316-da487d78fc26', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('84f26535-5fa3-489e-8316-da487d78fc26', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('84f26535-5fa3-489e-8316-da487d78fc26', foundational, coercion_overrides_revelation).
narrative_ontology:cs_axiom_status(coercion_overrides_revelation, holdable).
narrative_ontology:cs_axiom_grounding('84f26535-5fa3-489e-8316-da487d78fc26', coercion_overrides_revelation, empirically_contingent).
narrative_ontology:cs_axiom('84f26535-5fa3-489e-8316-da487d78fc26', secondary, theater_masks_suppression).
narrative_ontology:cs_axiom_status(theater_masks_suppression, holdable).
narrative_ontology:cs_axiom_grounding('84f26535-5fa3-489e-8316-da487d78fc26', theater_masks_suppression, conventional).
narrative_ontology:cs_reference_frame('84f26535-5fa3-489e-8316-da487d78fc26', divine_revelation_mandate).
narrative_ontology:cs_drift_state('84f26535-5fa3-489e-8316-da487d78fc26', post_federal_suppression_1890, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('84f26535-5fa3-489e-8316-da487d78fc26', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, polygamist_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, non_lds_territorial_settlers).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, protestant_cultural_establishment).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the Morrill Anti-Bigamy Act and subsequent legislation through prosecution, imprisonment, property seizure, and institutional pressure on the LDS church. Uses the coercive apparatus to eliminate plural marriage as a territorial normative practice. Achieves uniform legal-religious alignment: federal law and dominant Protestant cultural norms converge on monogamy-only, eliminating the competing moral claim plural marriage represented.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Practicing members who view plural marriage as a divine requirement (D&C 132 in LDS theology). Face federal prosecution, imprisonment (terms of 2–5 years), property confiscation, and social ostracism. Cannot exit without renouncing a central doctrine they understand as revealed, making exit fused to identity dissolution. The 1890 Manifesto (Official Declaration 1) pressures capitulation but does not resolve the theological claim — it asserts God commanded abandonment, creating cognitive dissonance for those who believe the prior revelation was also divine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, latter_day_saint_polygamists, payer,
    powerless, biographical, identity_locked, national).

% Spouses and children experience family dissolution under federal pressure: plural wives lose legal recognition, children lose inheritance protections, economic units fragment. Imprisonment of breadwinners collapses household income. Exit means family separation. The constraint operates through institutional dissolution of the family form itself, not merely penalizing the practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, polygamist_families, payer,
    powerless, biographical, trapped, national).

% Faces the choice between institutional survival (capitulation to federal authority, abandoning plural marriage doctrine) and institutional extinction (continued practice, confiscation of temple and property, imprisonment of leaders). The 1890 Manifesto is authored as a doctrinal revelation but functions as institutional capitulation to superior coercive power. Leadership collects organizational continuity and eventual statehood acceptance as the gain from the constraint; the cost is doctrinal integrity and moral authority in the eyes of strict-observance members.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership, payer).

% Non-LDS settlers and eastern interests benefit from federal enforcement as it eliminates a competing settlement and governance model. Plural marriage was read by federal authorities as a marker of LDS separatism and theocratic ambition; eliminating the practice signals integration into U.S. legal-religious uniformity. Non-LDS settlers gain territorial normalization and reduced institutional competition.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, non_lds_territorial_settlers, beneficiary,
    moderate, biographical, mobile, regional).

% Interpret and enforce the Morrill Act and Edmunds Act, upholding convictions and property seizures. Courts legitimate the suppression through constitutional and statutory reading, framing plural marriage as criminality rather than protected religious practice. The judiciary is the machinery through which federal intent becomes local enforcement.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would argue the 1890 Manifesto is not a legitimate revelation but a capitulation dressed in prophetic language. After 1890, many break away to continue plural marriage practice, viewing the institutional church as having betrayed the founding revelation. They remain structurally excluded from the institutional church's legitimacy narrative and subject to the same federal prosecution. Their theological position is the voice the constraint's framing attempts to silence.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, strict_observance_polygamists, excluded,
    powerless, biographical, identity_locked, regional).

% Monogamy-only is established Protestant theological and cultural orthodoxy. Federal enforcement of monogamy eliminates a competing moral-religious claim and consolidates Protestant cultural hegemony in the territories. The constraint vindicates the monogamy norm as natural law and superior moral principle.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, protestant_cultural_establishment, beneficiary,
    institutional, generational, analytical, national).

% Analyzes the structural relationship between federal coercion, institutional capitulation, and doctrinal reframing. Observes that the constraint persists through the framing (1890 Manifesto as revelation) rather than through explicit federal mandate — the victims are pressured into self-enforcement via doctrinal acceptance, theater is high.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, historical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no coordination function in this reading. Federal suppression of plural marriage coordinates the federal legal system with Protestant cultural norms, but this is asymmetric alignment, not reciprocal coordination. The constraint serves alignment of authority structures, not resolution of a collective-action problem shared by polygamists and federal authorities.
% TRANSFER_FUNCTION: Moves property, freedom of association, and doctrinal authority from LDS polygamists to the federal government and Protestant cultural establishment. Federal seizures of LDS property (under the Morrill Act, $1M+ confiscated by 1890) transfer material wealth directly. Doctrinal authority is transferred when the institutional LDS church is pressured to reframe plural marriage as no longer divinely mandated, ceding interpretive control to federal-sanctioned orthodoxy.
% ABSENT_VOICES: Strict-observance polygamists and women in plural marriages are the most structurally absent voices. Polygamist wives had no legal standing and minimal public platform; their testimony about the constraint's effects on families is systematically excluded from the official narrative. Strict-observance members who reject the 1890 Manifesto as illegitimate revelation are excluded from institutional legitimacy and face the same federal prosecution the church nominally escaped by capitulating.
% DISAPPEARANCE_RATIONALE: If federal enforcement of monogamy ceased, plural marriage would reappear immediately within LDS communities: theological commitment to D&C 132 persists beneath the 1890 Manifesto (evidenced by ongoing fundamentalist practice and the prevalence of plural marriage in contemporary polygamist breakaway communities). The constraint's disappearance would reshape territorial religious culture, LDS institutional hierarchy, and federal authority over religious practice.
% FOUNDING_PROBLEM: Federal authorities and Protestant cultural establishment identified plural marriage as a threat to territorial uniformity, legal standardization, and Protestant religious hegemony. The 'problem' was not a collective-action puzzle but a perceived deviance requiring suppression: plural marriage marked LDS separatism and theocratic ambition. The founding problem was thus the existence of a competing moral-legal system, not a coordination failure.
% FOUNDING_PROBLEM_CORROBORATION: Federal prosecutors and territorial officials explicitly testified that plural marriage was the target of suppression because it represented LDS institutional power and territorial control (see U.S. v. Reynolds, 1879; Congressional testimony on the Edmunds Act, 1882). Non-LDS contemporaries and federal authorities attest the 'problem' was LDS autonomy from federal jurisdiction, not a live threat to general social functioning. LDS institutional leadership, post-1890, reframes the founding problem retroactively as solved by the Manifesto, but this is self-serving. Strict-observance dissident voices attest that the theological problem (whether plural marriage is divinely mandated) was never solved — it was suppressed.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89) and rising through the interval because federal suppression confiscates property, imprisons breadwinners, and forces institutional capitulation — the cost to polygamists is severe and non-negotiable. Suppression is higher still (0.92) because the constraint persists through active federal enforcement (prosecution, imprisonment terms of 2–5 years, property seizure) and institutional self-enforcement (the church disciplining members who continue plural marriage). Theater ratio is notably high and rising sharply after 1890 (from 0.41 in 1882 to 0.62 in 1890, then 0.67–0.68 post-1890) because the suppression machinery shifts from explicit federal prosecution to institutional legitimation via the Manifesto: the illusion of prophetic reinterpretation substitutes for the explicit coercive narrative. The church becomes the enforcer of the federal constraint, making the suppression appear voluntary. Accessibility collapse is moderate (0.78) because strict-observance members demonstrably reject the collapse — they break away to continue plural marriage, showing that the constraint leaves alternatives (costly, identity-fusing alternatives, but real alternatives). Resistance is high (0.72) because polygamist communities mount legal challenges and organizational breakaway; the constraint does not suppress all opposition, though it suppresses organizational practice. The measurement grid is aligned across all three metrics at each time point.
 *
 * PERSPECTIVAL GAP:
 *   The LDS institutional leadership and the federal government experience opposite institutional outcomes from the same constraint, even though they both 'win' in the short term: the government achieves territorial uniformity (win); the church survives institutionally (win). But the government's win is structural and lasting (monogamy-only is codified as law), while the church's win is contingent and compromised (institutional survival comes at the cost of doctrinal incoherence and internal fissure). Polygamist victims experience the constraint as pure extraction: they lose religious freedom, family form, and doctrinal identity simultaneously. From the victim seat, the Manifesto is a betrayal framed as revelation. From the federal seat, the Manifesto is a capitulation framed as voluntary compliance — both framings are partially theater. The engine computes per-seat types: from the federal and Protestant seats, the constraint is a snare functioning smoothly; from the victim seats, it is a snare with high active resistance and identity-driven refusal to accept the constraint's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply by role. The federal government sits at d=0.0 (pure beneficiary: it collects territorial uniformity and property without bearing suppression costs). The LDS institutional leadership sits near d=0.5–0.6 (mixed): it survives institutionally (a gain) but sacrifices doctrinal integrity and internal legitimacy (a loss); it is both pressed by federal coercion and positioned to benefit from statehood acceptance once it capitulates. Polygamist victims sit at d=0.95–1.0 (pure targets): they bear imprisonment, property loss, family dissolution, and doctrinal betrayal without collecting any benefit from the constraint. The fed courts sit at d=0.0 (analytical seat, no directionality). Protestant establishment sits at d=0.0–0.2 (beneficiary: their norm is installed as law). Strict-observance dissidents sit at d=0.98 (targets: same federal prosecution plus institutional excommunication). These divergences flow from the declared victim/beneficiary structure and the exit_options modulation: powerless agents with identity_locked or trapped exit have high d; institutional beneficiaries with arbitrage or analytical exit have low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (eliminating plural marriage as a territorial practice) is structurally dead by 1904: federal authorities achieved their goal. Plural marriage ceased to be a live institutional practice in the LDS mainstream church. Yet the constraint persists through theatrical maintenance (the Manifesto's doctrinal language keeps the suppression machinery justified internally). The theater ratio and the gap between the constraint's functional extinction (plural marriage is institutionally gone) and its continued enforcement (dissidents are still prosecuted; the threat is still present) mark this as a candidate piton — a constraint whose primary function has atrophied but whose enforcement infrastructure persists because the narrative (revelation vs. capitulation) remains contested and because the beneficiary (federal territorial control) continues to accrue passive benefit from the suppression apparatus remaining in place. The constraint does not decay to zero theater because institutional voice remains that would testify to the true founding problem (federal coercion, not revelation): strict-observance dissidents keep alive the claim that the Manifesto is illegitimate, so the constraint must continue performing its revelation narrative to maintain institutional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_counterfactual,
    'Would the LDS institutional church have voluntarily reinterpreted plural marriage as no longer divinely mandated absent federal coercive pressure?',
    'Counterfactual historical analysis: comparison with other religious institutions that voluntarily adapted doctrines without external coercion, and analysis of LDS theological development absent federal suppression (evidenced by fundamentalist breakaway sects that continue plural marriage practice without pressure from federal authorities, showing theological commitment independent of coercion).',
    'If the answer is ''no'' — the church would have persisted in plural marriage absent federal pressure — the constraint is unambiguously snare (coercive extraction). If the answer is ''yes'' — the church would have reinterpreted voluntarily — the constraint reframes toward rope or tangled_rope (the coercion would have merely accelerated endogenous change). The reading assumes ''no,'' asserting the constraint is snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_counterfactual, conceptual, 'The causal role of federal coercion in the 1890 Manifesto — necessary cause, sufficient cause, or merely accelerating cause of change that was already internally inevitable.').

omega_variable(
    prophetic_authority_epistemic_base,
    'On what epistemological grounds can the LDS church distinguish between a genuine prophetic revelation (plural marriage in D&C 132) and a forced capitulation dressed as revelation (the 1890 Manifesto)?',
    'Theological analysis of LDS prophetology and testimony epistemology: what internal criteria distinguish divinely-mandated revelations from institutional capitulation framed prophetically? Interviews with LDS theologians, fundamentalist dissidents, and institutional leadership on the claimed sources of authority for each document.',
    'If no epistemic criteria exist within LDS theology to distinguish the two, the Manifesto is structurally irrefutable as ''revelation'' even if empirically coerced — the reading''s claim to observe coercion is relegated to external observers and dissidents. If criteria exist, the theological reading and the coercion reading become partially separable: one could argue both that coercion occurred AND that the Manifesto is a legitimate revelation (if LDS theology provides criteria for recognizing revelation under coercion). This omega records the committer frame''s acknowledgment that the kernel (plural_marriage_mandate) is read through incommensurable epistemologies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_authority_epistemic_base, conceptual, 'Whether LDS theology can internally distinguish prophetic revelation from coercively-framed institutional capitulation.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (external federal enforcement, threat of imprisonment and property seizure) or internalized (LDS members accepting the Manifesto as legitimate revelation and self-enforcing compliance)?',
    'Post-suppression trajectory analysis: after federal enforcement substantially decreased (post-1904, as Utah gained statehood and prosecution rates declined), did plural marriage practice reappear within mainstream LDS communities, or did doctrinal internalization persist? Evidence from fundamentalist breakaway sects and contemporary polygamist practice shows structural suppression remains operative (legal penalties, family dissolution) even absent federal prosecution. Pre-enforcement trajectory analysis (internal LDS documents, diaries, confessional records) would show the degree to which members believed plural marriage was divinely mandated independent of coercion.',
    'If suppression is primarily structural (the Manifesto is a capitulation forced by external threat), the constraint is a snare with high structural component. If suppression is primarily internalized (members genuinely accept the Manifesto''s revelation claim and police themselves), the constraint becomes more snare-with-theater: the coercion is now mediated through doctrinal acceptance, making exit require identity dissolution. In either case, the constraint persists through suppression, but the psychological mechanism differs — one involves active threat, the other involves cognitive dissonance managed through faith.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of plural marriage is maintained through external federal enforcement or through internalized LDS doctrinal acceptance of the Manifesto.').

omega_variable(
    federal_intent_religious_vs_territorial,
    'Was federal suppression of plural marriage motivated by genuine religious reform (a belief that monogamy is morally superior) or by territorial control (a need to eliminate LDS institutional autonomy and achieve federal jurisdiction over the territory)?',
    'Analysis of Congressional testimony and federal prosecutorial statements (1862–1890): explicit statements of motive. Comparison with federal treatment of other religious practices (e.g., Native American religious practices suppressed during the same period). Analysis of the correlation between plural marriage suppression and statehood admission: territories practicing plural marriage were denied statehood; once plural marriage was officially abandoned, Utah was admitted (1896). Fundamentalist researcher accounts of federal prosecutor intent.',
    'If motivation was genuinely religious-reform, the constraint might be framed as rope (imposing a norm deemed superior) with substantial theater. If motivation was territorial control, the constraint is clearly snare: the plural marriage issue is a pretext for establishing federal jurisdiction over an autonomous theocracy. This omega records the reading''s assumption that territorial control was the primary motivation, not moral reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_intent_religious_vs_territorial, empirical, 'Whether federal suppression was motivated by religious reform or by territorial control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.22).
narrative_ontology:measurement(plur_tr_t1872, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1872, 0.28).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.41).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.62).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.68).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.67).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.65).
narrative_ontology:measurement(plur_be_t1872, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1872, 0.72).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.81).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.87).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.89).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.58).
narrative_ontology:measurement(plur_su_t1872, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1872, 0.68).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.82).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.91).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__exogenous_override_reading, 0.15).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the plural_marriage_mandate kernel. The exogenous_override_reading asserts federal coercion as the structural cause of the 1890 Manifesto (snare, high extraction, high suppression). The endogenous_reinterpretation_reading asserts legitimate prophetic reinterpretation (rope, lower extraction, voluntary compliance). The institutional_pragmatism_reading asserts doctrinal framing serving institutional survival (tangled_rope, mixed extraction and coordination). All three readings share the same referent (the 1890 Manifesto as a constraint on LDS practice) but diverge on the causal mechanism (coercion vs. revelation vs. strategic adaptation). The readings are structurally linked: the exogenous_override reading influences both siblings by establishing coercive pressure as a competing explanation for the Manifesto's adoption; the endogenous_reinterpretation reading forecloses the exogenous reading's claim within LDS institutional theology (if revelation is genuine, coercion is secondary); the pragmatism reading coexists with exogenous but splits causality differently (both acknowledge coercion but frame it as occasion for reinterpretation rather than forcing mechanism). Each reading has distinct ε, victim set, and beneficiary set because they instantiate different causal structures from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
