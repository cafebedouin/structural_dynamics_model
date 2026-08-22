% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Fixity Discipline on Constitutional Interpretation
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   The United States Constitution is a contested kernel: a single persisting
 *   text read differently by rival interpretive authorities. This story
 *   instantiates ONE reading — the originalist reading — as a clean,
 *   epsilon-invariant constraint: the discipline under which constitutional
 *   meaning is fixed at ratification and judges must recover the original
 *   public understanding, with post-ratification practice counting only
 *   insofar as it evidences that meaning. The standing arrangement under
 *   contest — the interpretive regime in which historical evidence binds
 *   adjudication — is the epsilon referent, assessed from this story's
 *   analytical seat; the reading's endorsed alternative (a fully restored
 *   founding-era order) is NOT the referent. Sibling readings (living
 *   constitutionalist, positivist) are separate constraints with their own
 *   stories, linked through network.affects_constraints and
 *   cs_structure.reading_relations. Structurally the arrangement carries BOTH
 *   a genuine coordination function — a single fixed standard that resolves
 *   interpretive indeterminacy and disciplines judicial discretion — AND
 *   asymmetric extraction: rights claims lacking founding-era grounding are
 *   suppressed while the movement controlling the method converts doctrinal
 *   authority into institutional dominance. KEY AGENTS (by structural
 *   relationship): - supreme_court_originalist_majority: Agenda setter
 *   (institutional/identity_locked) — administers the history-and-tradition
 *   discipline; method and judicial identity are fused. -
 *   conservative_legal_movement: Primary beneficiary (powerful/constrained) —
 *   converts method dominance into appointments, clerkships, and scholarly
 *   authority. - federal_appointing_officials: Beneficiary and secondary
 *   agenda setter (institutional/mobile) — sets constitutional policy through
 *   nominations instead of Article V amendment. - modern_rights_litigants:
 *   Primary target (powerless/trapped) — bear suppression of claims lacking
 *   founding-era analogues. - civil_rights_advocacy_organizations: Target
 *   (organized/constrained) — mission-bound practitioners of disfavored
 *   doctrinal methods. - state_court_jurists: Inter-institutional target
 *   (institutional/constrained) — absorb harmonization pressure onto formally
 *   independent state texts. - living_constitutionalist_academics: Excluded
 *   voice (organized/constrained) — would contest method choice in the
 *   operative forum but sit outside it. - constitutional_historians: Evidence
 *   supplier and observer (organized/analytical) — produce the record the
 *   discipline consumes. - general_public: Dual-positioned (moderate/trapped)
 *   — receives rule-of-law predictability, bears foregone adaptive
 *   protection. - interpretive_theory_scholars: Analytical observer
 *   (analytical/analytical) — sees the full structure.
 *
 * KEY AGENTS:
 *   - supreme_court_originalist_majority: Agenda setter (institutional/identity_locked) — administers the discipline; bound by commitments fusing method with judicial identity
 *   - conservative_legal_movement: Primary beneficiary (powerful/constrained) — converts method dominance into appointments, clerkships, and chairs
 *   - federal_appointing_officials: Beneficiary and secondary agenda setter (institutional/mobile) — constitutional policy via nominations
 *   - modern_rights_litigants: Primary target (powerless/trapped) — claims without founding-era analogues fail
 *   - civil_rights_advocacy_organizations: Target (organized/constrained) — disfavored doctrinal toolkit, mission-bound
 *   - state_court_jurists: Inter-institutional target (institutional/constrained) — harmonization pressure on independent state texts
 *   - living_constitutionalist_academics: Excluded voice (organized/constrained) — outside the operative forum
 *   - constitutional_historians: Observer and evidence supplier (organized/analytical)
 *   - general_public: Dual-positioned beneficiary/payer (moderate/trapped)
 *   - interpretive_theory_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Fixity Discipline on Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'df1357c6-2334-4561-b758-0867203c5740').
narrative_ontology:cs_kernel_codification('df1357c6-2334-4561-b758-0867203c5740', fixed_text).
narrative_ontology:cs_authority_grounding('df1357c6-2334-4561-b758-0867203c5740', lineage).
narrative_ontology:cs_interpretation_layer_present('df1357c6-2334-4561-b758-0867203c5740').
narrative_ontology:cs_reading_relation('df1357c6-2334-4561-b758-0867203c5740', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('df1357c6-2334-4561-b758-0867203c5740', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('df1357c6-2334-4561-b758-0867203c5740', foundational, written_constitution_fixes_governable_content).
narrative_ontology:cs_axiom_status(written_constitution_fixes_governable_content, holdable).
narrative_ontology:cs_axiom_grounding('df1357c6-2334-4561-b758-0867203c5740', written_constitution_fixes_governable_content, conventional).
narrative_ontology:cs_axiom('df1357c6-2334-4561-b758-0867203c5740', foundational, judicial_fidelity_to_ratified_meaning_is_duty).
narrative_ontology:cs_axiom_status(judicial_fidelity_to_ratified_meaning_is_duty, holdable).
narrative_ontology:cs_axiom_grounding('df1357c6-2334-4561-b758-0867203c5740', judicial_fidelity_to_ratified_meaning_is_duty, deontological).
narrative_ontology:cs_reference_frame('df1357c6-2334-4561-b758-0867203c5740', fixed_ratification_era_public_meaning).
narrative_ontology:cs_drift_state('df1357c6-2334-4561-b758-0867203c5740', contemporary_post_dobbs_doctrine, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('df1357c6-2334-4561-b758-0867203c5740', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, federal_appointing_officials).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, supreme_court_originalist_majority).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, modern_rights_litigants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, state_court_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, general_public).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, general_public).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, original_public_meaning_methodology).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, popular_sovereignty_amendment_exclusivity).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, counter_majoritarian_difficulty_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the enforcing seat: decides which historical evidence counts, writes the history-and-tradition tests lower courts must follow, and overturns precedent that rested on nonoriginalist reasoning. Its members were selected through a pipeline that screened for methodological commitment, and their published opinions, legacies, and places in legal history are built on the method. Reversing course would mean repudiating their own life's work — exit from the commitment is personal and doctrinal apostasy, not a career move.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% A network of scholars, litigators, think tanks, and judges that spent four decades building the method's intellectual and personnel infrastructure. As the method became the enforcing standard, its members filled the bench, the clerkships, and the law-school chairs; conference invitations, citation networks, and confirmation prospects flow through method allegiance. Its accumulated capital — reputations, institutions, donor relationships — is invested in this particular standard, so pivoting to a rival method would forfeit most of it.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    powerful, generational, constrained, national).

% Presidents and senators who cannot amend the Constitution (Article V requires supermajorities they rarely command) but can set its practical meaning by choosing who interprets it. Nominations have become the principal channel of constitutional policy; each appointment locks in methodological direction for decades beyond the official's tenure. Their leverage ends when they leave office, which makes the appointment channel more valuable the shorter their horizons.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federal_appointing_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, federal_appointing_officials, agenda_setter).

% Individuals whose claims to liberty and equal treatment depend on changed circumstances, expanded moral premises, or government practices unimaginable at ratification — reproductive care, digital privacy, discrimination by institutions that did not exist in 1791. Under the enforcing standard, such claims must find an eighteenth- or nineteenth-century analogue or fail. They cannot exit the jurisdiction whose constitution governs them, and their access to relief runs entirely through courts committed to the standard.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, modern_rights_litigants, payer,
    powerless, biographical, trapped, national).

% Public-interest firms and advocacy groups whose doctrinal toolkit — substantive due process, evolving-standards arguments, purposive equal-protection readings — is now disfavored in the enforcing forum. They lose arguments they would have won a decade ago, watch settled protections roll back, and must re-frame claims under historical categories chosen by opponents. Their missions bind them to the clients and claims; leaving the forum means abandoning the people they serve.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, civil_rights_advocacy_organizations, payer,
    organized, biographical, constrained, national).

% Justices of state high courts interpreting their own constitutions. They retain formally independent texts, but the federal method exerts prestige and harmonization pressure: divergence invites criticism as unprincipled, while convergence imports federal historical tests into state traditions that never ratified them. Their exit — leaning fully on independent state-constitutional grounds — is available but costly in legitimacy and invites political retaliation through retention elections and appointments.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, state_court_jurists, payer,
    institutional, generational, constrained, national).

% Law professors and theorists who defend adaptive interpretation. They publish critiques, train students in methods the enforcing court now treats as disqualifying, and file the intellectual opposition. Their standing inside the operative forum has collapsed — fewer clerkships for their students, fewer invitations to testify, dwindling citation in opinions — though tenure insulates their salaries. They would contest the method choice directly if the forum admitted the argument.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_academics, excluded,
    organized, generational, constrained, national).

% Academic historians of the founding and nineteenth centuries whose archival work supplies the evidentiary record the discipline consumes. They watch their findings cited selectively — isolated passages, decontextualized glosses — and can object in journals and op-eds but hold no seat in adjudication. Demand for their expertise has risen sharply; control over how their work is used has not.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, constitutional_historians, observer,
    organized, generational, analytical, national).

% Citizens governed under the interpreted text. They receive what the method promises: rules that do not shift with judicial composition, a constitution that means what it said, and accountability that runs through elections and appointments rather than courtroom morality. They also bear the other side: protections that would have expanded with circumstances do not, and rights they may come to need have no founding-era analogue. They cannot exit the constitutional order at all.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_public, beneficiary,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, general_public, payer).

% Comparative legal theorists studying how courts across systems choose and justify interpretive methods. They examine testimony from every seat, compare jurisdictions, and analyze the method contest without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, interpretive_theory_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interpretive-indeterminacy problem: a polity governed under a two-century-old text needs a shared, stable standard for what it permits and forbids, so that officials, citizens, and courts coordinate expectations without each generation of judges re-authoring the Constitution. It also disciplines judicial discretion, converting open-textured clauses into historically bounded questions.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges and contemporary moral consensus to the ratification-era public; moves constitutional policy-setting power from an unreachable Article V supermajority to whoever controls judicial appointments; moves institutional rewards (seats, clerkships, chairs, citations) to practitioners of the method; moves rights-protection outcomes away from claimants whose claims presuppose changed circumstances.
% ABSENT_VOICES: Those excluded from the founding-era public whose understanding the method privileges — enslaved people, women, Indigenous nations — can register objections only as anachronisms, since the method weights voices by 1788-1868 participation. Contemporary claimants whose rights arguments rest on moral premises rejected at ratification stand outside the evidentiary frame entirely. They are absent from the operative forum: briefs arguing adaptive-interpretation premises now function as disqualifications rather than arguments in the enforcing court.
% DISAPPEARANCE_RATIONALE: If the fixity discipline vanished overnight, the enforcing majority's recent doctrine (built on history-and-tradition tests) would lose its warrant wholesale, appointment politics would reorganize around a different selection criterion, the movement's institutional pipeline would depreciate, and state-federal interpretive alignment would loosen — the constitutional order would rearrange around whichever method next captured the bench.
% FOUNDING_PROBLEM: Mid-twentieth-century critics charged that an assertive Supreme Court was deciding by judicial preference dressed as principle — the counter-majoritarian difficulty: unelected judges invalidating legislation on grounds untethered from text and history, making constitutional law the will of nine lawyers and corroding democratic legitimacy. The originalist discipline was built to bind judges to law rather than will.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the mid-century legitimacy crisis is documented across the spectrum — liberal critics of judicial supremacy (Alexander Bickel's least-dangerous-branch argument predates the movement) attested the discretion problem before the movement existed, and comparative-court scholarship continues to debate counter-majoritarian review independent of movement advocacy. What remains disputed, and why the status is contested rather than live, is whether judicial discretion or minority-rights protection is now the salient problem: the movement's own beneficiaries attest liveness, while rights-advocacy scholarship attests the cure now does more damage than the disease.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim — tangled_rope — states what I believe structurally true: the arrangement possesses a genuine coordination function (a single fixed interpretive standard resolving indeterminacy and disciplining discretion, which even hostile seats concede has rule-of-law value) AND asymmetric extraction routed through the same structure (claims lacking founding-era analogues are suppressed while the method's custodians convert doctrinal authority into appointments, clerkships, and chairs), held together by active enforcement (nominee screening, confirmation warfare, precedent-overturning). The metrics describe observed operation: extractiveness 0.68 reflects the widening gap between the arrangement's coordination yield and the rights-protection losses it imposes at interval end; suppression 0.78 reflects mature enforcement machinery — ideological screening of nominees, clerkship filters, demonstrated willingness to overturn precedent — noting suppression is a raw structural input the engine does not scale; theater_ratio 0.40 reflects the documented growth of outcome-selective historicism (law office history) alongside genuine archival scholarship; accessibility_collapse 0.60 marks that alternatives have largely collapsed inside the operative federal forum while remaining live in the academy and some state courts; resistance 0.62 marks sustained academic, state-court, and political counter-pressure. The measurement series share one six-point grid (1980-2025) across all three tracked metrics; trajectories are monotonic ratchets, not cycles — enforcement capacity and extraction accumulated together as the method captured the bench, so no cyclical-pattern analysis applies. Suppression mechanism: predominantly structural (appointment gates, forum control, precedent hierarchy — roughly seventy percent) with an internalized component (professional socialization equating serious lawyering with method allegiance — roughly thirty percent), which is what makes the enforcing seat's exit identity_locked rather than merely constrained. Coalition note: the least powerful target seat (individual litigants) aggregates its voice through the advocacy organizations, which is the main channel through which coalition power could alter the resistance trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the enforcing majority's seat the arrangement is the discipline it was selected to administer — fidelity, not extraction — and its identity lock makes the method self-vindicating. From the appointing officials' seat it is a policy channel substituting for an unreachable amendment process. From the movement's seat it is the return on forty years of infrastructure investment. From the litigants' and advocacy organizations' seats the same structure operates as a closing door: claims that would have succeeded under adaptive methods now fail on historical absence. State-court jurists experience it as harmonization pressure on formally independent texts. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (conservative_legal_movement, federal_appointing_officials, supreme_court_originalist_majority) drive those seats toward the beneficiary pole; victim declarations (modern_rights_litigants, civil_rights_advocacy_organizations, state_court_jurists) drive those seats toward the target pole, amplified by weak exit (trapped litigants, mission-constrained organizations, legitimacy-costly state divergence). One override: general_public is the only moderate-power seat, and deriving its directionality from its beneficiary declaration alone would pull it toward the subsidy end, ignoring that it equally bears the payer side (foregone adaptive protections); the override to 0.48 records the near-symmetric net position. Institutional seats need no override despite sharing a power atom: their differing beneficiary/victim declarations differentiate them within the structural derivation. Excluded and observer seats (living_constitutionalist_academics, constitutional_historians, interpretive_theory_scholars) contribute no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restraining judicial willfulness under an old text — is contested rather than dead: the movement attests it live, rights-advocacy scholarship attests the cure now outweighs the disease, and mid-century liberal critics corroborate that the discretion problem was real before the movement existed. Because the founding problem retains live disputants, the arrangement is not yet mandatrophy-resolved, and the classification guards against mislabeling in both directions: calling it a snare would erase the genuine coordination function (fixed standards, rule-of-law predictability) that even hostile seats concede; calling it a rope would erase the asymmetric, enforced extraction running through the same structure. The forward risk is piton: if the discretion problem dies — or comes to be universally judged superseded — while the enforcement machinery persists on inertia and selective-history performance, the arrangement would persist as performance administered by an agenda setter whose cost to fix exceeds what it bears. The identity_lock_durability omega tracks exactly that decay path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the kernel us_constitution_text (the originalist_reading). What would each sibling reading change structurally if it held the enforcing seat?',
    'Comparative classification across the three reading-stories linked in network.affects_constraints: diff the beneficiary/victim sets, epsilon, and enforcement profile of each reading''s file.',
    'Under the living constitutionalist reading the beneficiary/victim polarity reverses (rights-expanding coalitions collect; originalist-aligned actors bear the suppression); under the positivist reading the semantic-fixity content drops out entirely and extraction concentrates wherever enactment-procedure gatekeeping extracts. Cross-reading deltas locate the disagreement structurally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one kernel, three readings; this file is the originalist instantiation.').

omega_variable(
    fixity_natural_or_constructed,
    'Is the fixity of constitutional meaning a structural feature of written law generally (a near-mountain property of codified instruments), or a constructed disciplinary regime sustained by appointment control and professional enforcement?',
    'Cross-jurisdiction comparison: track whether written constitutions with no originalist enforcement machinery (state constitutions, foreign written constitutions, supranational texts) exhibit stable fixed meaning or drift with interpreter composition.',
    'If fixity is generic to written law, part of the measured extraction is the irreducible price of codified governance and the constraint sits closer to rope; if fixity-at-ratification is specifically constructed and enforced, the full extraction loads onto the enforcement apparatus and the constraint sits closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixity_natural_or_constructed, conceptual, 'Whether semantic fixity is a natural feature of written constitutionalism or a maintained construction.').

omega_variable(
    historical_evidence_determinacy,
    'Can original public understanding be recovered with enough determinacy to discipline adjudication, or is the historical record underdetermined enough that judges choose among candidate meanings under an originalist label?',
    'Code studies of history-and-tradition opinions: measure how often multiple defensible historical narratives were available and how the opinion selected among them; correlate selection with outcome valence.',
    'High underdetermination means theater_ratio is understated and the arrangement operates as discretion with historic dress, shifting classification toward snare; low underdetermination supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_determinacy, empirical, 'Determinacy of the historical evidence base the discipline consumes.').

omega_variable(
    selective_history_intrinsic_or_corruptible,
    'Is outcome-selective use of history (law office history) intrinsic to the method''s incentive structure, or a correctable defect removable by historiographic standards?',
    'Natural experiment: jurisdictions adopting professional-history panels or citation standards; measure whether selectivity declines without changing the outcome distribution.',
    'If intrinsic, part of the enforcement machinery is extraction infrastructure and the measured suppression is structurally necessary to the arrangement; if corruptible, the arrangement could shed its extractive component and migrate toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_history_intrinsic_or_corruptible, empirical, 'Whether selective historicism is built into the method or an incidental abuse.').

omega_variable(
    identity_lock_durability,
    'Does the enforcing majority''s commitment to the method survive personnel turnover, or does the arrangement''s persistence depend on the current cohort''s identity fusion with originalism?',
    'Track successor-cohort behavior after retirements: whether newly seated originalist-aligned justices maintain history-and-tradition discipline when it cuts against coalition interests.',
    'If commitment is cohort-specific, the arrangement decays toward piton as the founding cohort exits (performance without enforcement); if the method reproduces commitment across cohorts, extraction persists and hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity lock in the enforcing seat across personnel change.').

omega_variable(
    capture_vs_diffuse_gains,
    'Do the arrangement''s gains concentrate in the conservative legal movement, or diffuse across a broader originalist-aligned coalition (gun-rights litigants, religious-liberty organizations, deregulatory interests)?',
    'Trace doctrinal wins to organized sponsors: map which movements supplied the litigants, funding, and amicus infrastructure behind history-and-tradition victories.',
    'Concentration confirms the named gain_flow seat and the capture reading; broad diffusion would weaken the capture claim and redistribute effective directionality across the coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_diffuse_gains, empirical, 'Whether extraction receipts concentrate in the movement or diffuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_orig_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement_basis(us_const_orig_tr_t1980, observed).
narrative_ontology:measurement(us_const_orig_tr_t1990, us_constitution_text__originalist_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(us_const_orig_tr_t1990, observed).
narrative_ontology:measurement(us_const_orig_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(us_const_orig_tr_t2000, observed).
narrative_ontology:measurement(us_const_orig_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement_basis(us_const_orig_tr_t2010, observed).
narrative_ontology:measurement(us_const_orig_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement_basis(us_const_orig_tr_t2020, observed).
narrative_ontology:measurement(us_const_orig_tr_t2025, us_constitution_text__originalist_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(us_const_orig_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(us_const_orig_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement_basis(us_const_orig_be_t1980, observed).
narrative_ontology:measurement(us_const_orig_be_t1990, us_constitution_text__originalist_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement_basis(us_const_orig_be_t1990, observed).
narrative_ontology:measurement(us_const_orig_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement_basis(us_const_orig_be_t2000, observed).
narrative_ontology:measurement(us_const_orig_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement_basis(us_const_orig_be_t2010, observed).
narrative_ontology:measurement(us_const_orig_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(us_const_orig_be_t2020, observed).
narrative_ontology:measurement(us_const_orig_be_t2025, us_constitution_text__originalist_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(us_const_orig_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_const_orig_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement_basis(us_const_orig_su_t1980, observed).
narrative_ontology:measurement(us_const_orig_su_t1990, us_constitution_text__originalist_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement_basis(us_const_orig_su_t1990, observed).
narrative_ontology:measurement(us_const_orig_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement_basis(us_const_orig_su_t2000, observed).
narrative_ontology:measurement(us_const_orig_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement_basis(us_const_orig_su_t2010, observed).
narrative_ontology:measurement(us_const_orig_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(us_const_orig_su_t2020, observed).
narrative_ontology:measurement(us_const_orig_su_t2025, us_constitution_text__originalist_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(us_const_orig_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label constitutional interpretation conflates three structurally distinct constraints instantiating one kernel (us_constitution_text): the originalist reading (this file — fixity discipline with movement-capture extraction), the living constitutionalist reading (adaptive discipline with reversed beneficiary/victim polarity), and the positivist reading (enactment-procedure validity with no semantic-fixity content). Each gets its own epsilon, beneficiaries, and victims; they are linked here and via reading_relations rather than averaged into one story, per the epsilon-invariance principle. The upstream/downstream asymmetry runs from this file toward the others during the current enforcement phase: originalist dominance changes the legitimacy conditions under which the sibling readings compete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
