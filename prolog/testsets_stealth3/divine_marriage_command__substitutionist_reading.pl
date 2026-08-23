% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Post-Manifesto Mandatory Monogamy — Substitutionist Reading (Manifesto as New Revelation)
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   In 1890 the LDS church faced corporate dissolution, temple escheatment,
 *   and the imprisonment of its leadership under federal anti-polygamy
 *   enforcement; Wilford Woodruff issued the Manifesto ending the public
 *   practice of plural marriage, and the church canonized it as scripture.
 *   This story instantiates the substitutionist_reading of the
 *   divine_marriage_command kernel: the Manifesto as new revelation
 *   legitimately superseding the 1852 command, monogamy as the doctrinally
 *   required form, post-Manifesto plural marriage as apostasy, and the
 *   shift's institutional legitimacy resting on its framing as revelation
 *   rather than coercion. The epsilon referent is the standing arrangement
 *   under contest — the monogamy requirement together with its enforcement
 *   machinery (temple interviews, disciplinary councils, the apostasy
 *   designation) — assessed by this reading's own lights: the frame
 *   legitimizes obedience and discipline as covenant, while its own
 *   acknowledged costs (the sacrifice narrative for plural families, the
 *   eternal cutoff of practitioners) keep epsilon visible rather than
 *   near-zero. Sibling readings
 *   (divine_marriage_command__continuationist_reading,
 *   divine_marriage_command__coercion_visibility_reading) are separate
 *   constraint files linked through the network and author materially higher
 *   epsilon over the same referent. Claim and metrics are independent:
 *   claimed_type is my structural judgment (tangled_rope — genuine
 *   coordination function, asymmetric enforced extraction); the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - lds_first_presidency: agenda-setting beneficiary (institutional / identity_locked) — issued and canonized the Manifesto, administers enforcement, collects the legitimacy return; exit would dissolve the authority the office is
 *   - quorum_of_twelve_apostles: secondary enforcer with beneficiary position (institutional / constrained) — administers discipline; two members exited at severe personal cost, marking the seat's exit as costly but real
 *   - lds_general_membership: net beneficiary with payer residue (organized / constrained) — civic survival and integration; older plural-marriage families carry the transition cost
 *   - pre_manifesto_plural_families: primary target (powerless / identity_locked) — covenant status reclassified over them without consent
 *   - post_manifesto_practitioners: primary target (moderate / identity_locked) — apostasy designation, excommunication, criminal exposure; persist in organized schism
 *   - us_federal_government: external beneficiary (institutional / mobile) — collected compliance without administering the arrangement
 *   - secular_historians: analytical observer — documentary check on the revelation framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.45).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.65).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Post-Manifesto Mandatory Monogamy — Substitutionist Reading (Manifesto as New Revelation)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'd76ec226-d555-4792-94a6-3927da1e47da').
narrative_ontology:cs_kernel_codification('d76ec226-d555-4792-94a6-3927da1e47da', fixed_text).
narrative_ontology:cs_authority_grounding('d76ec226-d555-4792-94a6-3927da1e47da', lineage).
narrative_ontology:cs_interpretation_layer_present('d76ec226-d555-4792-94a6-3927da1e47da').
narrative_ontology:cs_reading_relation('d76ec226-d555-4792-94a6-3927da1e47da', divine_marriage_command__continuationist_reading, influences).
narrative_ontology:cs_reading_relation('d76ec226-d555-4792-94a6-3927da1e47da', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('d76ec226-d555-4792-94a6-3927da1e47da', foundational, manifesto_is_binding_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_is_binding_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('d76ec226-d555-4792-94a6-3927da1e47da', manifesto_is_binding_new_revelation, theological).
narrative_ontology:cs_axiom('d76ec226-d555-4792-94a6-3927da1e47da', secondary, post_manifesto_plural_marriage_is_apostasy).
narrative_ontology:cs_axiom_status(post_manifesto_plural_marriage_is_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('d76ec226-d555-4792-94a6-3927da1e47da', post_manifesto_plural_marriage_is_apostasy, conventional).
narrative_ontology:cs_reference_frame('d76ec226-d555-4792-94a6-3927da1e47da', continuing_revelation_supersession).
narrative_ontology:cs_drift_state('d76ec226-d555-4792-94a6-3927da1e47da', post_gospel_topics_essays_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d76ec226-d555-4792-94a6-3927da1e47da', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, lds_first_presidency).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, lds_general_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, us_federal_government).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, pre_manifesto_plural_families).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, post_manifesto_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, lds_general_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto and canonized it as scripture; declares which marriages are valid covenants, gates temple admission through interviews, and convenes disciplinary councils for members who marry plurally after the Manifesto. Collects the arrangement's central return: the Manifesto stands as the modern proof-case that the church president receives binding revelation, and the institution survived, kept its temples and property, and led Utah to statehood under this office's stewardship. There is no exit from the arrangement that leaves the office's authority intact — the framing is the authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, lds_first_presidency, agenda_setter,
    institutional, generational, identity_locked, global).

% Administers discipline alongside the First Presidency and staffs the enforcement machinery. Collects institutional continuity and the standing of the quorum. The arrangement reached into this seat itself: two members who had performed post-Manifesto plural marriages resigned under pressure in 1905-1907, showing that exit is possible but costs standing, livelihood, and eternal-salvation position within the community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, quorum_of_twelve_apostles, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, quorum_of_twelve_apostles, beneficiary).

% Belong to a church that survived, integrated civically, and now offers a monogamous covenant path with full civic standing. Most families joined after the change and experience the requirement simply as the church's marriage standard. Older families that had sacrificed for plural marriage carried a quieter cost: a principle they were taught was eternal was set aside by announcement, and their former covenant standing became something the institution explains rather than celebrates. Leaving is possible and happens, at the price of community and the eternal framework.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, lds_general_membership, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, lds_general_membership, payer).

% Entered plural marriage between 1852 and 1890 as the highest covenant the church then offered; husbands served prison terms, families split across households, some fled to Mexico and Canada colonies. After 1890 the same marriages became something the church no longer performs, no longer celebrates, and eventually disciplines when continued. The decision to set the practice aside was announced over these families without a seat at the table; they live with a covenant the institution performed and then renamed.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, pre_manifesto_plural_families, payer,
    powerless, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, pre_manifesto_plural_families, excluded).

% Continued or entered plural marriage after 1890, many believing the Manifesto suspended the practice under duress rather than ending it as doctrine. The church's own leadership performed or permitted some such marriages until 1904; after the Second Manifesto the same institution excommunicated members for what its own apostles had done years earlier. These members face disciplinary councils, loss of temple and membership standing, and, where state law reaches, criminal exposure. They persist in organized communities despite a century of excommunication.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, post_manifesto_practitioners, payer,
    moderate, generational, identity_locked, continental).

% Prosecuted plural marriage for a generation — dissolving the church corporation, confiscating property, imprisoning leaders — and achieved by the church's own announcement what continued prosecution might not have: public compliance and a state constitution banning plural marriage. It runs nothing inside the church; it collected the compliance outcome and stood down enforcement.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, us_federal_government, beneficiary,
    institutional, generational, mobile, national).

% Document the political and legal context of the 1890 decision from federal records, court filings, and Woodruff's private papers, and describe the gap between the official revelation account and the contemporaneous record. Their work is the standing outside check on the official narrative; they hold no stake in the church's covenant economy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, secular_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, lds_first_presidency).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a genuine collective-action problem: the church faced corporate dissolution, property escheatment, imprisonment of leaders, and the threatened cessation of temple ordinances under federal anti-polygamy enforcement. Mandatory monogamy aligned the community with the surrounding legal order, enabled Utah statehood, ended the criminal exposure of ordinary members, and standardized marriage practice across an expanding international membership.
% TRANSFER_FUNCTION: Moves covenant status and doctrinal legitimacy from plural-marriage practice to the institutional hierarchy's interpretive authority: the hierarchy gains the exclusive power to define which marriages are valid covenants and to reclassify that status retroactively, while plural families transfer their covenant standing, social honor, and legal security into institutional compliance. The doctrinal cost of the reversal — an 'eternal' command superseded — is displaced onto dissenters, who are reclassified as apostates rather than co-believers under duress.
% ABSENT_VOICES: The plural wives themselves — the people whose covenant marriages were reclassified — had no seat in the 1890 decision; the Manifesto was issued by the male hierarchy and ratified by conference acclamation. Their objection (that the covenant was divinely sanctioned and not the institution's to unmake) is structurally absent from the official narrative, which narrates their loss as sacrifice without their consent. Also absent: the practitioners who continued the practice, whose reading of the Manifesto is excluded from official discourse by the apostasy designation itself.
% DISAPPEARANCE_RATIONALE: If the monogamy requirement and its enforcement vanished overnight — if the church announced plural marriage doctrinally open — the institution would immediately re-collide with federal and state bigamy law; the legitimacy architecture built on the Manifesto as continuing revelation would face collapse, since a frame in which 1890 can be undone leaves no command stable; the fundamentalist movements would be vindicated as the faithful remnant and the apostasy boundary would invert; and the temple recommend system's marriage discipline would need reconstruction. The arrangement is load-bearing for the modern church's legal position, its authority structure, and its boundary system.
% FOUNDING_PROBLEM: Federal anti-polygamy enforcement at its apex: the Supreme Court had upheld corporate dissolution and temple escheatment, leaders were imprisoned or in hiding, and continued plural marriage threatened the cessation of temple ordinances and the church's legal existence. The arrangement was built to reconcile the covenant practice with the survival requirements of American legal integration — and, in this reading's frame, to implement God's timing for a doctrinal transition.
% FOUNDING_PROBLEM_CORROBORATION: The survival problem's reality is corroborated outside the beneficiary set: Supreme Court records (Late Corporation of the LDS Church v. United States, 1890), the Edmunds-Tucker Act's congressional record, and Woodruff's private papers as documented by secular historians (Hardy, Quinn, Flake). That the problem is now dead is attested by the absence of any federal legal challenge to LDS marriage practice for over a century and by the church's full civic integration. No beneficiary attests the status, and none needs to.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).
:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.45 is reading-indexed: the substitutionist frame converts most of the arrangement's costs into covenant and sacrifice categories, but its own documents acknowledge uncompensated losses (plural families whose highest-covenant marriages were reclassified by announcement) and it enforces an eternal cutoff on practitioners — a residue the frame prices but does not dissolve. Suppression 0.65 is a raw structural property, unscaled by power or scope: the excommunication machinery built between the 1904 Second Manifesto and the 1907 hearings, the temple interview gate, and the narrative management of the coercion record. Theater 0.45: the discipline is functional, but the revelation framing requires continuous maintenance against a documentary record showing fourteen years of senior-permitted post-Manifesto marriages after the 'revelation' — framing work that peaked in the correlation era (Goodhart drift: the story of the Manifesto becoming more central than its function) and eased partially after the 2013 essays. Accessibility collapse 0.60: inside the frame the alternative collapses to apostasy, but exit exists — a century of organized schismatic communities proves the alternative never fully closed. Resistance 0.60: elite noncompliance 1890-1904, the hearings as resistance made visible, and sustained schismatic persistence. The measurement series run on one shared grid (1890-2020, eight points, all three metrics at every point); trajectories are monotonic-with-plateau rather than cyclical — enforcement built to a mid-century plateau and eased after partial official acknowledgment, with no oscillation mechanism, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The presidency seat computes the arrangement as providence and duty: its authority is constituted by the framing, so from inside that seat no extraction is visible and the discipline reads as love. The plural-family and practitioner seats compute the same arrangement as imposed loss: a covenant revoked over them without consent, then criminalized as apostasy when continued. The federal seat computes a cheap policy victory — compliance collected without administration. Same-level divergence: the First Presidency and the Quorum of Twelve hold the same institutional power atom, but differentiated exits — two apostles resigned under pressure in 1905-1907 at severe personal cost, while the presidency has no exit that leaves the office intact; identity fusion, not power, differentiates the seats. Coalition check: the target classes persisted as communities for a century but never converted persistence into effective coalition power against the institution, because the institution held the identity infrastructure — temple, sealing, salvation — that the coalition's members could not replace.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the membership and the federal seat; the targets (plural families, practitioners) derive high directionality, pushed toward the full-target end by identity_locked exit. The presidency's seat is the interesting case: declared beneficiary, but identity_locked — the framing IS its authority, so exit equals self-destruction — and the exit modulation should pull its derived directionality up from the near-beneficiary end toward moderate, pricing its framing-maintenance burden and existential exposure. No directionality_overrides are authored: overrides key on the power atom, and the only institutional-atom correction needed (the presidency's fusion) is already captured by exit_options, while the federal seat — same atom, genuinely near-full-beneficiary — would be mis-corrected by any atom-level override. Effective extraction is the engine's computation from these declarations; suppression stays unscaled. Scope note: the institution operates at global scope while extraction concentrates on identity-locked targets at continental scope — the power-scope coupling concentrates costs on the least mobile seats, which is the coupling signature worth watching under identity_coordination's complexity offset.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabels. As pure snare it would erase the genuine coordination function: the survival problem was real, the solution worked, and the community is net-integrated — the arrangement is not cover all the way down. As pure rope it would erase the asymmetric, enforced extraction: identifiable targets, an apostasy boundary maintained by discipline, and gains that accrue to the authority seat. The R5 mismatch (dead founding problem + world_rearranges) is the load-bearing signal: the arrangement outlived its founding problem and now does legitimacy work — tangled_rope with a zombie mandate, not yet piton, because the function (boundary discipline, the continuing-revelation proof-case) is real and the theater is moderate. If framing maintenance fully detaches from function — sustained theater above 0.5, enforcement that disciplines only the already-marginal — expect drift toward piton with the presidency as the administrator for whom the fix is prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the substitutionist_reading of kernel divine_marriage_command. What structurally changes if a sibling reading is adopted instead, and where exactly is the disagreement located?',
    'Reading comparison across the kernel family: the continuationist_reading shifts the victim set (this reading''s apostates become the faithful remnant) and raises epsilon over the same referent (the covenant is treated as suspended under duress, not rescinded); the coercion_visibility_reading shifts the beneficiary structure (institutional survival becomes the naked gain, the revelation framing becomes cover) and raises epsilon further. The disagreement is located in the ontological status of the 1890 Manifesto — new revelation, prudential suspension, or coerced surrender — which determines who counts as apostate, what the enforcement is for, and the epsilon each reading authors.',
    'Adopting a sibling reading re-draws the apostasy boundary, re-aims the enforcement machinery, and materially raises the story''s epsilon; this reading authors the lowest epsilon of the three over the same standing arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one of three readings of the divine marriage command kernel; sibling adoption changes victims, enforcement aim, and epsilon.').

omega_variable(
    revelation_or_coerced_surrender,
    'Was the 1890 Manifesto a genuine revelation whose timing reflects providence, a coerced institutional surrender retroactively framed as revelation, or both at once — and does the 1890-1904 record of elite-permitted post-Manifesto marriages show the institution itself did not initially treat its own announcement as doctrinally operative?',
    'Documentary record: Woodruff''s private papers and contemporaneous correspondence as documented by secular historians, the 1904-1907 hearings transcript establishing that new plural marriages were performed with knowledge at senior levels after 1890, and the 2013 Gospel Topics essay''s partial acknowledgment of legal pressure. If the institution''s own conduct for fourteen years contradicted the framing, the pure-revelation account weakens substantially.',
    'If pure coercion, this reading collapses into the coercion_visibility_reading and the apostasy machinery loses its theological grounding — discipline for dissent becomes punishment for disagreement with a policy reversal. If revelation-through-coercive-circumstance, this reading holds but carries the supersession-coherence cost priced in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_or_coerced_surrender, empirical, 'Whether the Manifesto''s revelation framing tracks divine initiative, coercive necessity, or providence working through coercion.').

omega_variable(
    eternal_command_supersession_coherence,
    'Can the substitutionist frame stabilize the claim that an ''eternal and unchangeable'' command was legitimately superseded, without destabilizing prophetic authority itself — since a frame in which any command can be superseded makes every command, including the supersession, provisional?',
    'Theological development inside the frame: official treatments of conditional commandments and the 2013 essay''s framing; observe whether the frame produces a stable doctrine of supersession or leaves the 1886 ''I have not revoked this law'' declaration (the fundamentalists'' proof-text) unrebutted.',
    'If the frame stabilizes supersession, this reading''s lineage grounding holds. If not, the coherence cost migrates the reading toward the coercion_visibility_reading — authority increasingly maintained by narrative management rather than doctrine, with theater_ratio rising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternal_command_supersession_coherence, conceptual, 'Doctrinal coherence cost of superseding an eternal command inside the substitutionist frame.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the hold of the monogamy requirement on practitioners who continue plural marriage primarily structural (excommunication machinery, temple discipline, criminal law) or internalized (identity fusion with the covenant, such that exit equals damnation in their own frame)?',
    'Post-exit trajectory of the rare leavers from fundamentalist communities: if covenant-frame commitments persist long after the ecclesiastical and legal penalties lapse, the hold is substantially internalized; the century-long persistence of organized communities under continuous excommunication already suggests the identity lock outperforms the machinery.',
    'If substantially internalized, the authored suppression understates the constraint''s effective hold — the machinery is secondary to an identity lock the institution did not build and cannot switch off, and enforcement intensity is a poor proxy for the constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism for post-Manifesto practitioners.').

omega_variable(
    founding_function_drift,
    'The founding problem (federal coercion threatening institutional survival over plural marriage) is dead, yet the arrangement persists and its removal would rearrange the world — is the arrangement''s current work (legitimacy maintenance, the apostasy boundary, the continuing-revelation proof-case) a live coordination function or a zombie mandate kept alive by the fixer''s own prohibitive costs?',
    'Observe what the arrangement actually does with the founding problem absent: it disciplines present-tense marriage practice, maintains the apostasy boundary against schism, and anchors the prophetic-authority proof-case. If theater_ratio sustains above 0.5 while enforcement detaches from any live function, the zombie reading strengthens; if boundary discipline remains functionally load-bearing for membership coherence, the coordination reading holds.',
    'Supports tangled_rope over both pure-snare (the coordination function is real) and piton (the function is not yet mostly theatrical); if framing maintenance fully detaches from function, expect drift toward piton with the presidency as the administrator for whom the fix is prohibitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_function_drift, conceptual, 'Whether the arrangement''s post-survival function is live coordination or zombie mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(divi_tr_t1900, divine_marriage_command__substitutionist_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__substitutionist_reading, theater_ratio, 1904, 0.35).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__substitutionist_reading, theater_ratio, 1910, 0.38).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__substitutionist_reading, theater_ratio, 1935, 0.42).
narrative_ontology:measurement(divi_tr_t1960, divine_marriage_command__substitutionist_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(divi_tr_t1990, divine_marriage_command__substitutionist_reading, theater_ratio, 1990, 0.52).
narrative_ontology:measurement(divi_tr_t2020, divine_marriage_command__substitutionist_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.3).
narrative_ontology:measurement(divi_be_t1900, divine_marriage_command__substitutionist_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__substitutionist_reading, base_extractiveness, 1904, 0.45).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__substitutionist_reading, base_extractiveness, 1910, 0.5).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__substitutionist_reading, base_extractiveness, 1935, 0.52).
narrative_ontology:measurement(divi_be_t1960, divine_marriage_command__substitutionist_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(divi_be_t1990, divine_marriage_command__substitutionist_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(divi_be_t2020, divine_marriage_command__substitutionist_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement(divi_su_t1900, divine_marriage_command__substitutionist_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__substitutionist_reading, suppression_requirement, 1904, 0.6).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__substitutionist_reading, suppression_requirement, 1910, 0.72).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__substitutionist_reading, suppression_requirement, 1935, 0.75).
narrative_ontology:measurement(divi_su_t1960, divine_marriage_command__substitutionist_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(divi_su_t1990, divine_marriage_command__substitutionist_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(divi_su_t2020, divine_marriage_command__substitutionist_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The kernel divine_marriage_command (God commands marriage forms through the prophet) decomposes into three structurally distinct constraints over what the 1890 Manifesto IS: substitutionist_reading (this file — new revelation superseding the command; apostasy boundary; enforcement aimed at practitioners), continuationist_reading (prudential suspension; polygamy doctrinally valid; enforcement experienced as persecution), and coercion_visibility_reading (acknowledged coerced survival measure; the revelation framing as cover; epsilon highest). The epsilon values differ by a wide margin over the same referent because each reading prices the framing shift differently; per the epsilon-invariance principle they are separate files linked through affects_constraints rather than one story with a measurement parameter. The substitutionist reading sits downstream of the coercion record: the documentary evidence that pressures its framing is the same evidence the coercion_visibility reading takes as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
