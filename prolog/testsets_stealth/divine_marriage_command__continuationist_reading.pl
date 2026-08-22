% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Plural-Marriage Command (Post-Manifesto Splinter Regime)
 *   domain: religious/political-theology
 *
 * SUMMARY:
 *   After the 1890 Manifesto, the body that had received the plural-marriage
 *   command split in its self-understanding. This story instantiates the
 *   continuationist reading: the command of 1843 remains doctrinally valid
 *   and eternally binding; the Manifesto was a prudential suspension executed
 *   under federal duress, lacking rescissory force; practitioners who
 *   maintain plural marriage are theologically legitimate, and the
 *   fundamentalist splinter communities are the remnant preserving the
 *   original revelation. The operative arrangement this story is ABOUT — and
 *   the ε referent — is the standing continuationist practice regime as it
 *   actually exists: closed communities in which a prophet and council assign
 *   marriages, pool property and labor, and enforce compliance, all under
 *   external criminal prohibition. Per the kernel-reading ε rule, ε is
 *   authored for this standing arrangement with honest descriptive metrics,
 *   NOT for the fully free practice the reading endorses (that would drive ε
 *   toward zero for every advocacy reading) and NOT averaged across sibling
 *   readings. The claim/metric independence rule applies in full:
 *   claimed_type is authored as snare from my analytical seat because the
 *   coordination surface (community survival, mutual aid under persecution)
 *   is real but thinner than the extraction it channels, and persistence runs
 *   on exit-suppression; the metrics are authored independently as
 *   descriptive facts. The engine computes per-seat classifications from the
 *   structural data below. KEY AGENTS (by structural relationship): -
 *   continuationist_prophet_presidency: Agenda-setting authority
 *   (powerful/arbitrage) — assigns marriages, controls trust assets, collects
 *   tithed income; the seat the gains accrue to - priesthood_council_elders:
 *   Secondary administrators and collectors (organized/identity_locked) —
 *   receive wives and standing for loyalty - rank_and_file_believers:
 *   Dual-positioned members (moderate/identity_locked) — receive belonging
 *   and mutual aid, pay labor, tithing, and marriage autonomy -
 *   plural_wives_and_girls: Primary bearers of cost (powerless/trapped) —
 *   assigned, relocated, and bound without meaningful consent structures -
 *   expelled_adolescent_males: Bearers of cost by removal
 *   (powerless/constrained) — cast out to balance the marriage pool -
 *   excommunicated_dissenters: Cast-out objectors (moderate/mobile) — now
 *   outside, testifying and litigating - mainline_lds_church: Repudiating
 *   parent institution (institutional/analytical) — holds no seat in the
 *   councils its verdict defines - federal_state_prosecutors: External
 *   enforcer-analysts (institutional/analytical) — their raids and
 *   prosecutions supply the persecution narrative the internal authority
 *   feeds on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.87).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, snare).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Plural-Marriage Command (Post-Manifesto Splinter Regime)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political-theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '0dc8fc36-0119-46c1-93ce-02a54c8571e3').
narrative_ontology:cs_kernel_codification('0dc8fc36-0119-46c1-93ce-02a54c8571e3', fixed_text).
narrative_ontology:cs_authority_grounding('0dc8fc36-0119-46c1-93ce-02a54c8571e3', lineage).
narrative_ontology:cs_interpretation_layer_present('0dc8fc36-0119-46c1-93ce-02a54c8571e3').
narrative_ontology:cs_reading_relation('0dc8fc36-0119-46c1-93ce-02a54c8571e3', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0dc8fc36-0119-46c1-93ce-02a54c8571e3', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('0dc8fc36-0119-46c1-93ce-02a54c8571e3', foundational, manifesto_carries_no_revelatory_force).
narrative_ontology:cs_axiom_status(manifesto_carries_no_revelatory_force, holdable).
narrative_ontology:cs_axiom_grounding('0dc8fc36-0119-46c1-93ce-02a54c8571e3', manifesto_carries_no_revelatory_force, theological).
narrative_ontology:cs_axiom('0dc8fc36-0119-46c1-93ce-02a54c8571e3', foundational, plural_marriage_remains_eternally_binding).
narrative_ontology:cs_axiom_status(plural_marriage_remains_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('0dc8fc36-0119-46c1-93ce-02a54c8571e3', plural_marriage_remains_eternally_binding, theological).
narrative_ontology:cs_reference_frame('0dc8fc36-0119-46c1-93ce-02a54c8571e3', unrescinded_dc132_plural_marriage_covenant).
narrative_ontology:cs_drift_state('0dc8fc36-0119-46c1-93ce-02a54c8571e3', contemporary_post_second_manifesto_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0dc8fc36-0119-46c1-93ce-02a54c8571e3', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, continuationist_prophet_presidency).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, priesthood_council_elders).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_and_girls).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, expelled_adolescent_males).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, excommunicated_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, rank_and_file_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presides over the priesthood council, declares which men are worthy of additional wives, approves marriages including those of underage girls, and controls the land, homes, and businesses held in the communal trust. Teaches that obedience to his word is obedience to God. Imprisoned since 2011, he continues to direct the communities through recorded sermons played on rotation. Nothing about his position depends on the consent of those subject to it; his authority is constituted by the claim of an unbroken commission traceable to the original revelation, and every asset and allegiance the arrangement produces flows through his office.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, continuationist_prophet_presidency, agenda_setter,
    powerful, generational, arbitrage, regional).

% Administer wards, perform sealings, enforce dress and behavior codes, and report deviations upward to the prophet. Loyal elders receive additional wives, larger homes, and standing in the council hierarchy. Their marriages, families, livelihoods, and prospects of exaltation as they understand it all depend on the continuation claim being true; abandoning it would cost them everything they are permitted to have, so none of them tests it.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, priesthood_council_elders, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, priesthood_council_elders, agenda_setter).

% Born into the community, they receive schooling of uncertain accreditation, mutual aid in illness and hardship, and the promise of eternal family increase. They contribute tithes, unpaid construction labor on communal projects, and — in the case of daughters — compliance with marriage assignments. Leaving means losing every relationship they have, the only home they know, and, in the teaching they were raised on, their salvation. Some leave anyway, usually with help from former members on the outside.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, rank_and_file_believers, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, rank_and_file_believers, payer).

% Assigned to husbands by council decision, sometimes while still minors, they bear and raise large families and run the households whose labor supports the community. Most hold no personal documents, money, or independent education. Leaving requires crossing distance to towns where they know no one, surrendering contact with all their children under the community's custody norms, and accepting the teaching that they are damned for it. Those who escape typically do so through contact with former members who operate escape networks.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_and_girls, payer,
    powerless, biographical, trapped, regional).

% Removed from the community in their mid-teens, often for infractions as minor as speaking to a girl, so that older men may marry within the limited pool of eligible women. They are dropped in nearby towns without money, credentials, or family contact. Some later sue the communal trust for support; their removal is the arithmetic that lets the leadership's preferred marriage allocations proceed.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, expelled_adolescent_males, payer,
    powerless, immediate, constrained, regional).

% Former members who questioned assignments or leadership and were cast out and shunned, losing spouses and children who remained inside. They run support networks for escapees, testify in prosecutions, and litigate over trust assets. Their testimony is the principal channel through which conditions inside the communities become publicly known, and they are dismissed by the leadership as liars and apostates.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, excommunicated_dissenters, excluded,
    moderate, biographical, mobile, national).

% The parent body that issued the 1890 and 1904 declarations and now excommunicates any member who enters plural marriage. It publishes essays and cooperates with journalism and scholarship to distinguish itself from the splinter groups. Its verdict — that the Manifesto was binding and the command suspended — is precisely what the continuationist communities reject; it holds no seat in their councils and its authority claims are void inside them by definition.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainline_lds_church, excluded,
    institutional, civilizational, analytical, global).

% Enforce statutes against bigamous solemnization, unlawful conduct with minors, and benefits fraud in the communities. They mounted the 1953 Short Creek raid and the 2008 YFZ Ranch operation, and successive prosecutions have produced the movement's convictions. Their activity sets the external conditions under which the internal authority consolidates: every raid is narrated inward as persecution confirming the prophet's warnings.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_state_prosecutors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, continuationist_prophet_presidency).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a closed covenant community: allocates marriage partners within a scarce pool, pools property and labor through consecration to the trust, reproduces the population rapidly, and binds members to the group against a surrounding legal order that prohibits the practice.
% TRANSFER_FUNCTION: Moves marriage decisions, reproductive capacity, domestic labor, construction labor, and tithed income from ordinary members — disproportionately women and adolescents — upward to the prophet and council, who control assignment, housing, and doctrine; moves legitimacy claims downward to believers in exchange for compliance.
% ABSENT_VOICES: Expelled teenagers and divorced-out wives are outside the walls by design; women awaiting assignment have no independent advocate inside; the mainline church speaks publicly but is dismissed as apostate and holds no seat; child-welfare and constitutional scholarship enters only through prosecutions, never through internal deliberation.
% DISAPPEARANCE_RATIONALE: If the continuation claim and its enforcement vanished overnight, the remaining communities would merge into the mainline church or disperse into surrounding towns; marriage assignments would stop; the trust's property would revert to ordinary ownership; and the century-old apparatus of raids, prosecutions, and litigation would lose its object within a generation.
% FOUNDING_PROBLEM: The original command organized marriage among a convert-heavy frontier church and welded a persecuted people into a distinct covenant nation; the continuationist arrangement was built to preserve that command intact after the 1890 surrender, so that the fullness of the priesthood would survive the parent institution's retreat.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: historians of the period corroborate the demographic and political context of nineteenth-century plural marriage; the mainline church's own published essays concede the historical origins and circumstances; trial records from successive prosecutions document the arrangement's operations. No source outside the continuationist communities attests that the founding problem — preservation of an eternally binding command — remains live; that liveness is asserted only from within, which is itself the signal the corroboration rule exists to catch.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement concentrates marriage assignment, reproductive capacity, domestic and construction labor, and tithed income under a single authority whose allocations are decoupled from the welfare of those allocated. Suppression is higher still (0.87) and is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled downstream. The suppression is compound: physical isolation, withheld documentation, custody norms that make exit mean losing every child, shunning, and wife-and-child reassignment as penalty, layered over teaching from birth that departure is damnation. Theater ratio is low-to-moderate (0.25) and its TRAJECTORY is the interesting datum: the series starts high (0.55 at the Manifesto itself — an era of public compliance coexisting with privately performed plural marriages, the most theatrical phase of the whole history) and declines steadily as the community physically separates, because enforcement becomes substantively costly rather than performative. Residual theater persists in the persecution liturgy and in governance by recorded sermon from prison. Accessibility_collapse (0.60) is honest to a split condition: inside the epistemic frame, alternatives collapse nearly completely (monogamy is apostasy; departure is damnation); from outside, the frame leaks through ex-member networks and prosecution publicity, so collapse is incomplete. Resistance (0.70) is sustained: a century of prosecutions, the 1953 Short Creek raid, the 2008 YFZ operation, class litigation by expelled members, and episodic mass exits. COALITION CHECK: the victim seats are individually powerless but have shown coalition capacity — expelled-teenager lawsuits against the trust, coordinated testimony after the 2008 raid, and matured ex-member exit infrastructure — yet coalition formation is blunted by identity lock and geography, which is precisely why suppression rather than poverty alone holds the structure. CYCLICAL PATTERN: the series shows a crisis-consolidation cycle rather than monotonic drift — external crackdown (1953, 2008) is followed by internal hardening, because each raid validates the persecution narrative on which the prophet's authority runs; the oscillation is partially constitutive, an intermittent-reinforcement dynamic in which the constraint's external enemy performs part of the internal enforcement work. Base_properties values are sampled at interval end (T=135), the post-fragmentation phase after the founder's imprisonment, when slight decay appears in both extractiveness and suppression as communities scatter.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structure. From the prophet's seat the arrangement is sacred order he administers at arbitrage-grade advantage — nothing about his position depends on anyone's consent, and the gains demonstrably accrue to his seat. From the wives' and expelled boys' seats the same structure is enforced extraction with no exit. From the believers' seat it is a covenant that costs heavily but returns belonging, aid, and salvific meaning — near-symmetric, slightly on the paying side. From the prosecutors' seat the practice is simply crime, and from the mainline church's seat it is apostasy; both sit outside the internal economy and experience the arrangement as an object of enforcement or repudiation rather than as their own constraint. Same-level divergence: elders and ordinary believers hold the same nominal community standing, but access to assignment authority and trust assets differentiates their exits — the elder's identity lock is anchored in rewards he would forfeit, the believer's in penalties he would suffer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection: the prophet collects tithing, labor, and assignment prerogative (d near the beneficiary end, amplified by arbitrage exit); elders collect wives and standing (low d, identity-locked). Victim declarations map to borne cost: wives and girls bear assignment, relocation, and domestic extraction with trapped exit (d near the full-target end); expelled boys bear removal itself (high d). Believers are genuinely dual-positioned, hence the directionality override: the structural derivation reads the beneficiary declaration and would damp their effective extraction toward subsidy, but the derived value is wrong — they pay tithing, labor, and marriage autonomy while receiving belonging and aid, and their identity lock amplifies the cost side. The override sets d = 0.48, just past symmetric onto the paying side. Prosecutors and the mainline church are observer/excluded seats outside the internal d-economy; their roles record their structural position without feeding the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the original command — organizing marriage among a convert-heavy frontier church and welding a persecuted people into a covenant nation — is practically dead; its demographic and political conditions dissolved a century ago. The continuationist reading is itself a mandatrophy-preserving device: it keeps the mandate alive by reframing the suspension as persecution, converting obsolescence into proof of faithfulness. Classifying this as snare rather than rope prevents the mislabel that voluntary-religion framing invites (membership is nominally voluntary; exit is structurally and spiritually catastrophic), and classifying the kernel contest as three linked constraints rather than one prevents the mislabel that the dispute is a single question with a single answer. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the parties dispute whether the founding problem lives, but no one disputes that arrangements depend on the structure — if it vanished overnight the communities dissolve, assignments stop, and the persecution economy loses its object. That combination flags a mandate sustained by contested genealogy rather than demonstrated function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_revelatory_status,
    'Was the 1890 Manifesto presented and received as revelation from God, or as administrative advice issued under legal duress?',
    'Documentary audit: Woodruff''s private diaries and papers, contemporaneous conference transcripts, the deliberately non-declarative wording of Official Declaration 1 itself, and the post-1890 record of secret plural marriages performed with leadership knowledge.',
    'If the record supports the prudential reading, the continuationist premise gains textual plausibility and the substitutionist sibling weakens; if the record shows a revelatory claim later abandoned, continuationism rests on denying the tradition''s own documented course.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_revelatory_status, empirical, 'Revelatory versus administrative status of the 1890 Manifesto — the load-bearing historical premise of this reading.').

omega_variable(
    duress_historicity,
    'Was the suspension genuinely produced by overwhelming and unavoidable federal coercion (property confiscation, disenfranchisement, imprisonment of practitioners), such that compliance was the only survivable path?',
    'Legislative history of the Edmunds-Tucker Act, church financial and legal-counsel records of the period, and counterfactual analysis of available resistance strategies.',
    'The duress premise is the wall that holds up the prudential-suspension framing. If duress was survivable or exaggerated, the suspension looks freely chosen, the persecution narrative that stabilizes internal authority loses its fuel, and this reading''s account of why the command went unpracticed collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_historicity, empirical, 'Whether the historical duress that supposedly forced the suspension was real and overwhelming.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is member retention governed mainly by structural barriers (geographic isolation, poverty, withheld documents, custody norms) or by internalized ones (taught damnation outside the group, identity fused from birth with the covenant people)?',
    'Post-exit trajectory studies: whether escapees report the fear and obligation dissipating once outside (internalized component) or report durable practical barriers (structural component); comparison of exit rates before and after ex-member support networks matured.',
    'If predominantly internalized, effective suppression exceeds the structural measure and travels with the member after exit; remediation and exit-infrastructure investment matter more than legal change. If structural, physical intervention (policing, services, litigation) dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    priesthood_line_validity,
    'Do the fundamentalist ordination lines (the claimed 1886 commissions and their 1929 organizational successors) trace validly to pre-Manifesto apostolic authority by the tradition''s own validity criteria?',
    'Genealogical audit of ordination certificates, witness chains, and dates against the validity standards the tradition itself applies to the mainline''s succession claims.',
    'If the lines are broken, the continuation claim reduces to charisma and the authority structure loses its distinctive warrant, collapsing toward ordinary leadership; if intact by internal criteria, the legitimacy dispute with the mainline is genuinely unresolved and the splinter authority claim stands on equal footing within the tradition''s own rules.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priesthood_line_validity, empirical, 'Validity of the unbroken-commission claim on which splinter authority rests.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the divine_marriage_command kernel; the three readings disagree specifically on the revelatory status and consequent authority of the 1890 Manifesto — which structural element carries the disagreement, and what does each sibling resolution do to this reading?',
    'Locate the dispute in the Manifesto''s status rather than in the command''s content: evidence about 1890 events (documents, diaries, legal history) moves all three readings, in opposite directions. A substitutionist resolution reassigns practitioner legitimacy to violation and dissolves the splinter authority claim; a coercion-visibility resolution legitimizes the mainline settlement and strands the splinters as schismatics; this reading survives only if the Manifesto lacks rescissory force.',
    'Determines which sibling reading is displaced by historical evidence and whether the victim/beneficiary sets of the family invert (under substitutionism, the practitioners of this story become the violating party).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: where the kernel contest is located and what each sibling resolution would do to this reading''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(divi_tr_t14, divine_marriage_command__continuationist_reading, theater_ratio, 14, 0.48).
narrative_ontology:measurement(divi_tr_t39, divine_marriage_command__continuationist_reading, theater_ratio, 39, 0.38).
narrative_ontology:measurement(divi_tr_t63, divine_marriage_command__continuationist_reading, theater_ratio, 63, 0.33).
narrative_ontology:measurement(divi_tr_t94, divine_marriage_command__continuationist_reading, theater_ratio, 94, 0.26).
narrative_ontology:measurement(divi_tr_t118, divine_marriage_command__continuationist_reading, theater_ratio, 118, 0.22).
narrative_ontology:measurement(divi_tr_t135, divine_marriage_command__continuationist_reading, theater_ratio, 135, 0.25).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(divi_be_t14, divine_marriage_command__continuationist_reading, base_extractiveness, 14, 0.64).
narrative_ontology:measurement(divi_be_t39, divine_marriage_command__continuationist_reading, base_extractiveness, 39, 0.7).
narrative_ontology:measurement(divi_be_t63, divine_marriage_command__continuationist_reading, base_extractiveness, 63, 0.74).
narrative_ontology:measurement(divi_be_t94, divine_marriage_command__continuationist_reading, base_extractiveness, 94, 0.79).
narrative_ontology:measurement(divi_be_t118, divine_marriage_command__continuationist_reading, base_extractiveness, 118, 0.83).
narrative_ontology:measurement(divi_be_t135, divine_marriage_command__continuationist_reading, base_extractiveness, 135, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(divi_su_t14, divine_marriage_command__continuationist_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(divi_su_t39, divine_marriage_command__continuationist_reading, suppression_requirement, 39, 0.66).
narrative_ontology:measurement(divi_su_t63, divine_marriage_command__continuationist_reading, suppression_requirement, 63, 0.74).
narrative_ontology:measurement(divi_su_t94, divine_marriage_command__continuationist_reading, suppression_requirement, 94, 0.84).
narrative_ontology:measurement(divi_su_t118, divine_marriage_command__continuationist_reading, suppression_requirement, 118, 0.91).
narrative_ontology:measurement(divi_su_t135, divine_marriage_command__continuationist_reading, suppression_requirement, 135, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the divine_marriage_command kernel. One colloquial label — 'the 1890 Manifesto ended plural marriage' — covers three structurally distinct claims that differ on the Manifesto's revelatory status and therefore on who is legitimate: this continuationist story (command still binds; suspension void; practitioners legitimate; ε authored for the standing splinter practice regime), the substitutionist story (new revelation; monogamy required; post-1904 practitioners are the violating party — the victim/beneficiary sets invert), and the coercion-visibility story (survival necessity legitimizes the mainline settlement; the splinters strand as schismatics). Each carries its own ε, victims, and classification per the ε-invariance principle; evidence about 1890 events moves all three in opposite directions, which is why they are modeled as linked family members rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
