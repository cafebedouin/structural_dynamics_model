import json
d=json.load(open('outputs/pipeline_output.json'))
out={}
for rec in d['per_constraint']:
    cid=rec['id']; vj=rec.get('verdict_join') or {}
    out[cid]=f"claimed={rec.get('claimed_type')}|sig={rec.get('signature')}|join={vj.get('verdict')}|base={vj.get('base_verdict')}|grade={vj.get('signature_grade')}"
for cid in sorted(out): print(f"{cid} :: {out[cid]}")
