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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Plural Marriage Command, Continuationist Reading: Doctrine Intact, Practice Suspended Under Duress
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   The kernel is the plural-marriage command of 1843 (Doctrine & Covenants
 *   132), announced as binding divine law in 1852 and suspended — on all
 *   sides' accounts under federal duress — by the 1890 Manifesto. This story
 *   instantiates ONE reading of that kernel, the continuationist reading: the
 *   command was never rescinded; the Manifesto is a prudential suspension
 *   under government coercion that left the doctrine and the sealing
 *   authority intact; the fundamentalist communities organized from the 1920s
 *   onward claim continuity with the original revelation through the claimed
 *   1886 ordination line. The standing arrangement under contest (the epsilon
 *   referent) is that continuationist practice regime: plural marriage under
 *   priesthood assignment, continued covertly and then openly in enclave
 *   communities while federal and state law prohibit it. Epsilon is
 *   calibrated from the continuationist seat: the reading sincerely holds the
 *   covenant and attributes much of the community's cost to external duress,
 *   so epsilon sits well below what a secular abolitionist seat would author
 *   for the same arrangement; it is not near zero because the arrangement's
 *   internal cost structure — who is assigned in marriage, which sons are
 *   expelled, who collects tithing and deference — is visible from inside,
 *   and the reading itself concedes the community lives under duress. The
 *   sibling readings are separate constraints with their own epsilon: the
 *   substitutionist reading (monogamy now required; the Manifesto is
 *   superseding revelation) authors epsilon over a monogamous arrangement
 *   with a different, far smaller victim set; the coercion-visibility reading
 *   authors epsilon over the same episode with legitimacy relocated from
 *   revelation to institutional survival. The claimed type and the metrics
 *   are independent authored facts: this story claims tangled_rope and
 *   authors its metrics as descriptively true; the engine computes per-seat
 *   classifications from the structural data. Authoring assumptions: interval
 *   t=0 corresponds to 1890 and t=135 to 2025; sibling constraint IDs are
 *   assumed to follow the kernel__reading pattern used here; provenance
 *   commit hashes record the prompt and schema versions in effect for this
 *   generation.
 *
 * KEY AGENTS:
 *   - priesthood_leadership: Agenda-setter and principal beneficiary (institutional / identity_locked) — holds the claimed unbroken keys, allocates marriages, collects tithing and deference, bears personal criminal exposure.
 *   - senior_plural_husbands: Beneficiary (organized / identity_locked) — receive wives, household labor, and standing; carry prosecution risk.
 *   - rank_and_file_believers: Beneficiary with payer burden (moderate / identity_locked) — receive community, mutual aid, and promised exaltation; pay tithing, forfeit sons, carry legal risk.
 *   - plural_wives_and_daughters: Primary target (powerless / identity_locked) — placed into marriage by assignment; bear the domestic, reproductive, and legal burden.
 *   - surplus_young_men: Target (powerless / trapped) — expelled as the marriage arithmetic tightens; dropped outside with nothing.
 *   - apostates_and_expelled: Excluded voice (powerless / constrained) — would testify against the arrangement; their testimony is dismissed in advance as apostasy.
 *   - federal_government: Excluded external coercer (institutional / mobile) — criminalized and prosecuted the practice; speaks by statute and raid, not deliberation.
 *   - mainstream_lds_church: Excluded rival claimant (institutional / mobile) — holds the substitutionist reading; excommunicates continuationists and denies their keys.
 *   - mormonism_scholars: Analytical observer (analytical / analytical) — documents the underground and the communities from outside all claimant traditions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.8).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Plural Marriage Command, Continuationist Reading: Doctrine Intact, Practice Suspended Under Duress").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'd6d82a9c-5918-4162-bab5-456132ef306a').
narrative_ontology:cs_kernel_codification('d6d82a9c-5918-4162-bab5-456132ef306a', fixed_text).
narrative_ontology:cs_authority_grounding('d6d82a9c-5918-4162-bab5-456132ef306a', lineage).
narrative_ontology:cs_interpretation_layer_present('d6d82a9c-5918-4162-bab5-456132ef306a').
narrative_ontology:cs_reading_relation('d6d82a9c-5918-4162-bab5-456132ef306a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('d6d82a9c-5918-4162-bab5-456132ef306a', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('d6d82a9c-5918-4162-bab5-456132ef306a', foundational, dc132_remains_binding_law).
narrative_ontology:cs_axiom_status(dc132_remains_binding_law, holdable).
narrative_ontology:cs_axiom_grounding('d6d82a9c-5918-4162-bab5-456132ef306a', dc132_remains_binding_law, theological).
narrative_ontology:cs_axiom('d6d82a9c-5918-4162-bab5-456132ef306a', foundational, manifesto_prudential_not_doctrinal).
narrative_ontology:cs_axiom_status(manifesto_prudential_not_doctrinal, holdable).
narrative_ontology:cs_axiom_grounding('d6d82a9c-5918-4162-bab5-456132ef306a', manifesto_prudential_not_doctrinal, empirically_contingent).
narrative_ontology:cs_axiom('d6d82a9c-5918-4162-bab5-456132ef306a', secondary, unbroken_1886_sealing_authority).
narrative_ontology:cs_axiom_status(unbroken_1886_sealing_authority, holdable).
narrative_ontology:cs_axiom_grounding('d6d82a9c-5918-4162-bab5-456132ef306a', unbroken_1886_sealing_authority, theological).
narrative_ontology:cs_reference_frame('d6d82a9c-5918-4162-bab5-456132ef306a', dc132_perpetually_binding).
narrative_ontology:cs_drift_state('d6d82a9c-5918-4162-bab5-456132ef306a', post_manifesto_criminalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d6d82a9c-5918-4162-bab5-456132ef306a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, priesthood_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, senior_plural_husbands).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_and_daughters).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, surplus_young_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, rank_and_file_believers).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, perpetual_validity_of_dc132).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, unbroken_1886_sealing_keys).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the sealing keys the community recognizes as unbroken from the 1843 revelation through the claimed 1886 ordination. Authorizes which marriages may be performed, assigns wives, adjudicates loyalty, and teaches that the 1890 Manifesto suspended practice under government pressure without touching the command. Collects tithing, labor, and deference; decides who marries whom and who may remain. Its senior figures have gone to prison for the practice. Renouncing the keys claim would dissolve the authority that constitutes the office; from where it stands, that renunciation is unbelief, not an option.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, priesthood_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Men holding two or more wives assigned through priesthood channels. They receive household labor, children, and elevated standing in the promise of exaltation. They also carry prosecution risk, tithing obligations, and the community's collective legal jeopardy. Leaving would forfeit family, standing, and salvation as they understand it; their framework offers no path to the promised end except staying.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, senior_plural_husbands, beneficiary,
    organized, biographical, identity_locked, regional).

% Ordinary members raised inside the covenant. They receive mutual aid under persecution, a total identity, and the promised exaltation conditioned on obedience. They pay tithing, surrender sons to expulsion when the marriage arithmetic tightens, and carry criminal liability for a practice their leaders authorize. Exit is imaginable only as damnation and total family loss; most cannot construct a self outside the covenant.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, rank_and_file_believers, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, rank_and_file_believers, payer).

% Women and girls placed into marriages by priesthood assignment rather than their own choosing, including minors during the recent one-man-rule era. They bear the domestic and reproductive load of the arrangement, restricted schooling, isolation from outside institutions, and the public stigma and legal exposure of the practice. Leaving means losing children, parents, community, and, as taught, salvation itself.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_and_daughters, payer,
    powerless, biographical, identity_locked, local).

% Teenage boys expelled or driven out, disproportionately as the number of marriageable women per man tightens. They lose family, community, and the only social world they know, and are dropped into an outside world for which their upbringing prepared them poorly. Some later testify against the leadership; re-entry is foreclosed.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, surplus_young_men, payer,
    powerless, biographical, trapped, regional).

% Former members who left or were cast out. They hold direct knowledge of the community's internal workings and many would testify against the arrangement, but inside the covenant conversation their voices count in advance as apostasy. Ongoing entanglement — children and parents still inside, shunning, custody — keeps their departure costly and incomplete.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, apostates_and_expelled, excluded,
    powerless, biographical, constrained, regional).

% The United States government, which from the Morrill Act through the Edmunds-Tucker Act criminalized plural marriage, imprisoned practitioners, seized church property, and disenfranchised members, and which raided continuationist settlements in 1953 and 2008. It is the external force the continuationist reading names as duress. It holds no seat in the covenant's deliberations; its objection takes the form of statute, prosecution, and raid rather than argument.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_government, excluded,
    institutional, generational, mobile, national).

% The Utah-based church, which holds that the Manifesto was revelation superseding the plural-marriage command and that the fundamentalist claim to unbroken keys is void. It excommunicates continuationist practitioners and denies the validity of their sealings. It is the rival claimant to the same kernel and is not a participant in this community's deliberations.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_lds_church, excluded,
    institutional, generational, mobile, global).

% Historians and scholars of new religious movements who document the post-Manifesto underground, the fundamentalist schisms, and the communities' internal structure from outside all the claimant traditions. They take no side in the covenant and hold no stake in its continuation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mormonism_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, priesthood_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded covenant community under external criminalization: a single recognized authority allocates marriages, transmits the founding revelation and its claimed key-line across generations, organizes mutual aid among members who face prosecution and stigma, and holds the group's boundary against assimilation into the surrounding society.
% TRANSFER_FUNCTION: Moves marriageable women, and their reproductive and domestic labor, from their natal households to priesthood-assigned husbands; moves tithing, labor, and obedience from the membership upward to the leadership; moves legal risk onto every practitioner while status, authority, and the promised exaltation concentrate among senior men; moves sons out of the community when the marriage arithmetic tightens.
% ABSENT_VOICES: Expelled young men, women removed or reassigned, apostates, and the federal state would all object, and none holds a seat: the first three are classified in advance as apostasy or persecution, and the state speaks through prosecution rather than deliberation. The unanimity the community displays is produced partly by these absences.
% DISAPPEARANCE_RATIONALE: The continuationist communities are built around the arrangement: their marriage order, their authority structure, their identity, and their mutual-aid economy all presuppose the covenant's continuation. Overnight disappearance would dissolve those communities into the surrounding society — the mainstream church's own post-1890 course shows the alternative path — while prosecutors, scholars, and the rival church would lose the object they engage.
% FOUNDING_PROBLEM: How can a people who received plural marriage as an everlasting command remain the covenant people when the state criminalizes the command — how to keep the principle, the authority, and the community intact through a suspension imposed from outside.
% FOUNDING_PROBLEM_CORROBORATION: The duress itself is corroborated outside the benefiting parties: prosecution and enforcement records (Morrill, Edmunds, and Edmunds-Tucker enforcement; the 1953 Short Creek raid; the 2008 YFZ raid), contemporaneous church presidency statements, and scholarship by historians with no continuationist allegiance all attest the coercion context. The claim that the command remains live and binding, and that the sealing keys continue unbroken, is attested only within the continuationist communities themselves — no source outside the benefiting parties corroborates the continuity claim, and that absence is itself signal.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.65: the arrangement coordinates a real community — mutual aid under prosecution, a total identity, continuity of a founding revelation across five generations — and simultaneously concentrates its costs on those with no seat at the allocation table: wives are assigned, minors have been placed into marriages, sons are expelled when the marriage arithmetic tightens, and tithing and legal risk flow upward while standing concentrates among senior men. Suppression is 0.80, authored as a raw structural property of the arrangement's own enforcement (the engine, not the story, scales extractiveness by directionality and scope): expulsion, shunning, near-total information control in the one-man-rule era, and the teaching that refusal is damnation. Theater is 0.40: the practice itself is functionally real, but a substantial share of activity is performance — authority-genealogy narratives around the 1886 ordination, staged obedience and confession under surveillance, public purity maintained around concealed or contested practice. Accessibility collapse is 0.62: the alternatives (life outside, monogamy, apostasy) are visible to every member but are priced as damnation and total family loss, so they collapse only partially. Resistance is 0.68: apostasy waves, expelled sons' testimony, state raids (1953 Short Creek, 2008 YFZ), litigation, and scholarship all press against the arrangement. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the machinery built up across the interval, hardening sharply in the one-man-rule consolidation. The measurement series share one time grid (t in years since 1890) so every metric is authored at every point; the trajectory is cyclical rather than monotonic — each external crackdown (1904 hearings, 1953 raid, 2008 raid) spikes solidarity and lets leadership renew sacrifice demands, followed by quiet accumulation until the next crackdown. The oscillation is partly an extraction mechanism in itself (intermittent reinforcement: persecution periodically re-validates the covenant and re-prices exit) and partly exogenous enforcement politics. Base_properties values describe the arrangement at interval end (t=135).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different arrangements from the same structure. From the leadership seat this is a covenant it stewards at genuine personal cost — its senior figures have gone to prison for it — and every demand it makes is authorized by the keys it holds; from the wives' seat the same structure is assignment without consent under information control, with exit priced as damnation. Rank-and-file believers sit genuinely between: they receive real goods (mutual aid, identity, meaning under persecution) and pay real costs (sons, tithing, criminal exposure) — the same person can sincerely testify the community is a blessing and have lost a son to expulsion. Coalition potential among the powerless is real on paper (wives and expelled sons outnumber the leadership many times over) but is suppressed by isolation, information control, and the identity frame; when it has briefly surfaced — expelled sons testifying, women fleeing the 2008 raid — it has moved prosecutors, not the covenant. The engine computes this divergence per seat from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: priesthood_leadership and senior_plural_husbands sit near the beneficiary end (d low) — the arrangement allocates wives, labor, deference, and authority to them. Victim declarations: plural_wives_and_daughters and surplus_young_men sit near the target end (d high) — they bear the arrangement's costs without holding allocation power. Rank-and-file believers are declared beneficiaries because they genuinely collect community goods, but their net position is near-symmetric — they tithe, forfeit sons, and carry criminal liability — so a directionality override sets moderate-power agents to d = 0.45; the derivation from beneficiary membership alone would have read them as near-full beneficiaries, which their own situation contradicts. The federal government and the mainstream LDS church are deliberately declared neither beneficiary nor victim: the federal state is the external coercer the reading itself names as duress, and the LDS church is the rival claimant to the same kernel; both take the canonical fallback for their power atoms, which is honest — neither collects from this arrangement nor pays into it in the covenant's own currency. Receipt surface: the gains demonstrably accrue to the priesthood_leadership seat — tithing, labor, marriage-allocation control, deference — so gain_flow names that seat rather than 'diffuse'. Fixing cost: the only actor with authority to release the community is the leadership whose authority IS the covenant's continuity, and external attempts to fix it by force have repeatedly failed at enormous cost, so fixing_cost is prohibitive.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible. A pure-extraction reading of fundamentalist polygamy would erase the genuine coordination — mutual aid, identity, continuity under persecution — that explains why members stay voluntarily and why suppression alone has never dissolved the communities; policy built on the pure-extraction reading (the raids) has repeatedly failed on exactly that point. A pure-coordination reading would erase who pays: the same structure that binds the community assigns wives without consent and expels sons. The R5 genealogy: the founding problem (preserving a criminalized command's community through suspension) is live within the reading, and the founding-problem status and disappearance verdict agree (live + world_rearranges), so no zombie mismatch fires — the arrangement's persistence is not inertial theater; it is actively maintained by people who believe it. The open question the omegas carry is whether the authority claim is corroborated outside the benefiting parties (keys_continuity_corroboration): if it is not, the coordination function remains real but its administration is self-asserted hierarchy, and the arrangement drifts toward the snare end. Identity-lock dynamics: the binding is ideological and relational — covenant identity from birth, exaltation theology that prices exit as damnation, and, for the leadership, professional identity in which the authority IS the keys claim. If the identity frame broke at scale (as it partially did in the late-twentieth-century apostasy waves and the post-2008 prosecutions), enforcement capacity would collapse and the arrangement would drift toward theatrical maintenance over a shrinking base rather than hold its current form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the divine_marriage_command kernel instantiates the operative constraint for a given community — the unrescinded command (continuationist), superseding revelation (substitutionist), or survival-derived legitimacy (coercion-visibility)?',
    'Corpus-level comparison of the three sibling stories: victim sets, epsilon, and computed types diverge structurally; membership claims, prosecution records, and community self-descriptions show which reading each community actually lives.',
    'The substitutionist reading removes the lost-boys and placement-marriage victim classes entirely and drops epsilon sharply; the coercion-visibility reading relocates legitimacy from revelation to institutional survival, changing the enforcement story from covenant discipline to institutional self-preservation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is the continuationist reading of a three-reading kernel; sibling readings are separate constraints with different epsilon and victim sets.').

omega_variable(
    manifesto_doctrinal_force,
    'Was the 1890 Manifesto doctrinally inert — a prudential suspension with no force against the command itself — or doctrinally efficacious from issuance?',
    'The contemporaneous record: Woodruff''s own statements about what the document did, plural sealings authorized by church presidents after 1890, the changed language of the 1904 Second Manifesto, and Smoot-hearings testimony.',
    'If doctrinally efficacious at 1890, the continuationist authority chain breaks there and the arrangement reclassifies from continuous covenant to post-1890 schismatic innovation — changing whose keys, if anyone''s, the agenda-setter holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_doctrinal_force, empirical, 'Whether the Manifesto touched the doctrine or only the practice.').

omega_variable(
    keys_continuity_corroboration,
    'Is the claimed unbroken line of sealing authority — the 1886 ordination and its transmission to the present one-man councils — corroborated by any source outside the continuationist benefiting parties?',
    'Documentary history independent of the Council of Friends'' own transmission: contemporaneous records, cross-testimony among rival fundamentalist factions, and court records.',
    'If uncorroborated, the agenda-setter''s authority is self-asserted; the coordination function remains real but its administration is naked hierarchy, and the arrangement''s computed classification drifts toward the snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(keys_continuity_corroboration, empirical, 'Whether the keys-continuity claim has any non-beneficiary attestation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of members structural (economic dependency, isolation, custody, expulsion machinery) or internalized (covenant identity from birth, damnation-terror that persists after exit)?',
    'Post-exit suppression trajectory of leavers: if damnation-fear, shunning entanglement, and skill deficits persist after the enforcement machinery is out of reach, the internalized share is substantial.',
    'If substantially internalized, the arrangement''s effective suppression exceeds the structural measure and survives external intervention — raids remove the machinery but not the suppression members carry with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the covenant communities.').

omega_variable(
    duress_attribution,
    'Of the costs the arrangement imposes, how much is attributable to external federal duress — as the reading itself insists — and how much to internal hierarchical capture that would persist without any external pressure?',
    'Comparative and counterfactual evidence: plural-marriage communities under decriminalization or in other jurisdictions; post-receivership trajectories of the raided communities; whether wife-assignment and expulsion practices track external pressure or internal consolidation.',
    'If internal capture dominates, the reading''s duress frame functions as a legitimacy shield for internal extraction, epsilon rises toward the snare end, and the reading''s own account of its costs becomes part of the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_attribution, conceptual, 'How the arrangement''s costs split between external duress and internal hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_cont_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(dmc_cont_tr_t0, observed).
narrative_ontology:measurement(dmc_cont_tr_t15, divine_marriage_command__continuationist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(dmc_cont_tr_t15, observed).
narrative_ontology:measurement(dmc_cont_tr_t30, divine_marriage_command__continuationist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(dmc_cont_tr_t30, observed).
narrative_ontology:measurement(dmc_cont_tr_t45, divine_marriage_command__continuationist_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(dmc_cont_tr_t45, observed).
narrative_ontology:measurement(dmc_cont_tr_t63, divine_marriage_command__continuationist_reading, theater_ratio, 63, 0.24).
narrative_ontology:measurement_basis(dmc_cont_tr_t63, observed).
narrative_ontology:measurement(dmc_cont_tr_t78, divine_marriage_command__continuationist_reading, theater_ratio, 78, 0.28).
narrative_ontology:measurement_basis(dmc_cont_tr_t78, observed).
narrative_ontology:measurement(dmc_cont_tr_t90, divine_marriage_command__continuationist_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement_basis(dmc_cont_tr_t90, observed).
narrative_ontology:measurement(dmc_cont_tr_t105, divine_marriage_command__continuationist_reading, theater_ratio, 105, 0.38).
narrative_ontology:measurement_basis(dmc_cont_tr_t105, observed).
narrative_ontology:measurement(dmc_cont_tr_t120, divine_marriage_command__continuationist_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement_basis(dmc_cont_tr_t120, observed).
narrative_ontology:measurement(dmc_cont_tr_t135, divine_marriage_command__continuationist_reading, theater_ratio, 135, 0.4).
narrative_ontology:measurement_basis(dmc_cont_tr_t135, observed).

% Extraction over time
narrative_ontology:measurement(dmc_cont_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(dmc_cont_be_t0, observed).
narrative_ontology:measurement(dmc_cont_be_t15, divine_marriage_command__continuationist_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(dmc_cont_be_t15, observed).
narrative_ontology:measurement(dmc_cont_be_t30, divine_marriage_command__continuationist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(dmc_cont_be_t30, observed).
narrative_ontology:measurement(dmc_cont_be_t45, divine_marriage_command__continuationist_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement_basis(dmc_cont_be_t45, observed).
narrative_ontology:measurement(dmc_cont_be_t63, divine_marriage_command__continuationist_reading, base_extractiveness, 63, 0.58).
narrative_ontology:measurement_basis(dmc_cont_be_t63, observed).
narrative_ontology:measurement(dmc_cont_be_t78, divine_marriage_command__continuationist_reading, base_extractiveness, 78, 0.54).
narrative_ontology:measurement_basis(dmc_cont_be_t78, observed).
narrative_ontology:measurement(dmc_cont_be_t90, divine_marriage_command__continuationist_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement_basis(dmc_cont_be_t90, observed).
narrative_ontology:measurement(dmc_cont_be_t105, divine_marriage_command__continuationist_reading, base_extractiveness, 105, 0.63).
narrative_ontology:measurement_basis(dmc_cont_be_t105, observed).
narrative_ontology:measurement(dmc_cont_be_t120, divine_marriage_command__continuationist_reading, base_extractiveness, 120, 0.66).
narrative_ontology:measurement_basis(dmc_cont_be_t120, observed).
narrative_ontology:measurement(dmc_cont_be_t135, divine_marriage_command__continuationist_reading, base_extractiveness, 135, 0.65).
narrative_ontology:measurement_basis(dmc_cont_be_t135, observed).

% Suppression requirement over time
narrative_ontology:measurement(dmc_cont_su_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(dmc_cont_su_t0, observed).
narrative_ontology:measurement(dmc_cont_su_t15, divine_marriage_command__continuationist_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(dmc_cont_su_t15, observed).
narrative_ontology:measurement(dmc_cont_su_t30, divine_marriage_command__continuationist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(dmc_cont_su_t30, observed).
narrative_ontology:measurement(dmc_cont_su_t45, divine_marriage_command__continuationist_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(dmc_cont_su_t45, observed).
narrative_ontology:measurement(dmc_cont_su_t63, divine_marriage_command__continuationist_reading, suppression_requirement, 63, 0.72).
narrative_ontology:measurement_basis(dmc_cont_su_t63, observed).
narrative_ontology:measurement(dmc_cont_su_t78, divine_marriage_command__continuationist_reading, suppression_requirement, 78, 0.66).
narrative_ontology:measurement_basis(dmc_cont_su_t78, observed).
narrative_ontology:measurement(dmc_cont_su_t90, divine_marriage_command__continuationist_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement_basis(dmc_cont_su_t90, observed).
narrative_ontology:measurement(dmc_cont_su_t105, divine_marriage_command__continuationist_reading, suppression_requirement, 105, 0.82).
narrative_ontology:measurement_basis(dmc_cont_su_t105, observed).
narrative_ontology:measurement(dmc_cont_su_t120, divine_marriage_command__continuationist_reading, suppression_requirement, 120, 0.85).
narrative_ontology:measurement_basis(dmc_cont_su_t120, observed).
narrative_ontology:measurement(dmc_cont_su_t135, divine_marriage_command__continuationist_reading, suppression_requirement, 135, 0.8).
narrative_ontology:measurement_basis(dmc_cont_su_t135, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, federal_anti_bigamy_enforcement).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto and plural marriage' covers three structurally distinct claims about what the 1890 document did to the 1843 command; per the epsilon-invariance principle they are three constraints, not one constraint with a measurement parameter. This file (continuationist: doctrine intact, practice suspended, keys continue) authors epsilon over a continuing plural-marriage practice regime. divine_marriage_command__substitutionist_reading authors epsilon over a monogamous arrangement in which the command was superseded — a different, much smaller victim set (no lost boys, no placement marriages). divine_marriage_command__coercion_visibility_reading authors epsilon over the same historical episode with legitimacy relocated from revelation to institutional survival necessity. The federal anti-bigamy enforcement regime (federal_anti_bigamy_enforcement) is the external constraint all three readings take as given and is the duress the continuationist reading names.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
